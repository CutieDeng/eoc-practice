// gccast: serialize GCC GENERIC trees to a Racket-readable .dat file.
//
// Copyright (C) 2026 cutiedeng
//
// This file is part of gccast-plugin.  gccast-plugin is free software:
// you can redistribute it and/or modify it under the terms of the GNU
// General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
// See LICENSE in this directory.
//
// Design (see doc/roadmap.md, Phase 6.1 of the compiler.rkt repo):
//   - PLUGIN_PRE_GENERICIZE fires once per function with DECL_SAVED_TREE
//     still in front-end GENERIC form (C/C++ *_STMT nodes intact).
//   - Each function is serialized immediately into its own node table
//     (pointer -> index map lives only during that call, so GC reuse of
//     addresses between functions cannot alias nodes).
//   - Only version-stable accessors are used here.  Per-version facts
//     (tree-code set, operand counts, operand names) are harvested by
//     gen/treedef.rkt into a .rktd table that the Racket reader uses to
//     decode operand positions; GCCAST_GCC_VERSION comes from the
//     generated gccast-config.inc.
//
// Output grammar (Racket `read`able, prefab structs):
//   #s(TU gcc-version main-file (fn ...))
//   fn   := #s(Fn "name" decl-ref body-ref #(node ...))
//   node := #s(Node code type-ref loc fields)
//   ref  := #s(TreeId n) | #f
//   loc  := ("file" line col) | #f
//   fields := alist; expressions carry ((ops ref ...)), decls/types/
//             constants/exceptional nodes carry named entries.  Decl
//             chains are materialized only where a construct owns them
//             (BIND_EXPR vars, TYPE_FIELDS, DECL_ARGUMENTS, BLOCK vars).

#include "gcc-plugin.h"
#include "plugin-version.h"
#include "tree.h"
#include "tree-iterator.h"
#include "real.h"

#include <cstdio>
#include <cstring>
#include <string>
#include <vector>
#include <unordered_map>

#include "gccast-config.inc"

int plugin_is_GPL_compatible;

namespace {

std::string g_out_path;
std::string g_fns;          // accumulated #s(Fn ...) records
int g_fn_count = 0;

// ---------------------------------------------------------------- text

void put_escaped(std::string &o, const char *s, size_t n, bool bytes) {
  for (size_t i = 0; i < n; i++) {
    unsigned char c = (unsigned char)s[i];
    if (c == '"' || c == '\\') { o += '\\'; o += (char)c; }
    else if (c == '\n') o += "\\n";
    else if (c == '\t') o += "\\t";
    else if (c == '\r') o += "\\r";
    else if (c < 0x20 || c == 0x7f || (bytes && c >= 0x80)) {
      char buf[8]; snprintf(buf, sizeof buf, "\\x%02x", c); o += buf;
    } else o += (char)c;
  }
}
void put_string(std::string &o, const char *s) {
  o += '"'; put_escaped(o, s, strlen(s), false); o += '"';
}
void put_bytes(std::string &o, const char *s, size_t n) {
  o += "#\""; put_escaped(o, s, n, true); o += '"';
}
void put_int(std::string &o, long long v) { o += std::to_string(v); }
void put_bool(std::string &o, bool b) { o += b ? "#t" : "#f"; }

// ---------------------------------------------------------------- walker

struct Serializer {
  std::unordered_map<tree, int> index;
  std::vector<std::string> nodes;
  int depth = 0;

  // Returns the node index for t, serializing it on first visit.
  int visit(tree t) {
    if (!t) return -1;
    auto it = index.find(t);
    if (it != index.end()) return it->second;
    int id = (int)nodes.size();
    index.emplace(t, id);
    nodes.emplace_back();            // placeholder: fields may refer back to id
    std::string text;
    if (++depth > 20000) {
      text = "#s(Node too_deep #f #f ())";
    } else {
      serialize(t, text);
    }
    --depth;
    nodes[id] = std::move(text);
    return id;
  }

  void ref(std::string &o, tree t) {
    int id = visit(t);
    if (id < 0) o += "#f"; else { o += "#s(TreeId "; put_int(o, id); o += ')'; }
  }
  void refs_chain(std::string &o, tree t) {     // TREE_CHAIN-linked list -> (ref ...)
    o += '(';
    bool first = true;
    for (tree x = t; x; x = TREE_CHAIN(x)) { if (!first) o += ' '; first = false; ref(o, x); }
    o += ')';
  }
  void field(std::string &o, const char *key) { o += " ("; o += key; o += " . "; }
  void field_ref(std::string &o, const char *key, tree t) { field(o, key); ref(o, t); o += ')'; }
  void field_int(std::string &o, const char *key, long long v) { field(o, key); put_int(o, v); o += ')'; }
  void field_bool(std::string &o, const char *key, bool b) { field(o, key); put_bool(o, b); o += ')'; }
  void field_str(std::string &o, const char *key, const char *s) { field(o, key); put_string(o, s ? s : ""); o += ')'; }
  void field_chain(std::string &o, const char *key, tree t) { field(o, key); refs_chain(o, t); o += ')'; }

  void location(std::string &o, tree t) {
    location_t loc = UNKNOWN_LOCATION;
    if (DECL_P(t)) loc = DECL_SOURCE_LOCATION(t);
    else if (EXPR_P(t) && EXPR_HAS_LOCATION(t)) loc = EXPR_LOCATION(t);
    if (loc == UNKNOWN_LOCATION) { o += "#f"; return; }
    expanded_location xl = expand_location(loc);
    o += '(';
    put_string(o, xl.file ? xl.file : "");
    o += ' '; put_int(o, xl.line); o += ' '; put_int(o, xl.column);
    o += ')';
  }

  void serialize(tree t, std::string &o) {
    enum tree_code code = TREE_CODE(t);
    enum tree_code_class cls = TREE_CODE_CLASS(code);
    o += "#s(Node ";
    o += get_tree_code_name(code);
    o += ' ';
    if (CODE_CONTAINS_STRUCT(code, TS_TYPED) && code != IDENTIFIER_NODE) ref(o, TREE_TYPE(t)); else o += "#f";
    o += ' ';
    location(o, t);
    o += " (";
    switch (cls) {
      case tcc_declaration: decl(t, code, o); break;
      case tcc_type:        type(t, code, o); break;
      case tcc_constant:    constant(t, code, o); break;
      case tcc_exceptional: exceptional(t, code, o); break;
      default:              operands(t, code, o); break;   // unary/binary/comparison/reference/statement/vl_exp/expression
    }
    o += "))";
  }

  void operands(tree t, enum tree_code code, std::string &o) {
    o += "(ops";
    int n = TREE_OPERAND_LENGTH(t);
    for (int i = 0; i < n; i++) { o += ' '; ref(o, TREE_OPERAND(t, i)); }
    o += ')';
    if (TREE_SIDE_EFFECTS(t)) field_bool(o, "side-effects", true);
    // Decl chains are not emitted on the decls themselves (a namespace-
    // scope decl's chain is every sibling declaration, builtins included);
    // the one GENERIC construct that owns a decl chain spells it out here.
    if (code == BIND_EXPR) field_chain(o, "vars", BIND_EXPR_VARS(t));
  }

  void decl(tree t, enum tree_code code, std::string &o) {
    o += "(uid . "; put_int(o, DECL_UID(t)); o += ')';
    tree name = DECL_NAME(t);
    field(o, "name");
    if (name && TREE_CODE(name) == IDENTIFIER_NODE) put_string(o, IDENTIFIER_POINTER(name)); else o += "#f";
    o += ')';
    field_bool(o, "public", TREE_PUBLIC(t));
    field_bool(o, "static", TREE_STATIC(t));
    field_bool(o, "external", DECL_EXTERNAL(t));
    field_bool(o, "artificial", DECL_ARTIFICIAL(t));
    field_bool(o, "readonly", TREE_READONLY(t));
    field_bool(o, "volatile", TREE_THIS_VOLATILE(t));
    field_ref(o, "size", DECL_SIZE(t));
    switch (code) {
      case VAR_DECL:
      case CONST_DECL:
        field_ref(o, "initial", DECL_INITIAL(t));
        break;
      case FUNCTION_DECL:
        field_chain(o, "arguments", DECL_ARGUMENTS(t));
        field_ref(o, "result", DECL_RESULT(t));
        break;
      case FIELD_DECL:
        field_ref(o, "offset", DECL_FIELD_OFFSET(t));
        field_ref(o, "bit-offset", DECL_FIELD_BIT_OFFSET(t));
        field_bool(o, "bit-field", DECL_BIT_FIELD(t));
        break;
      case PARM_DECL:
        field_ref(o, "arg-type", DECL_ARG_TYPE(t));
        break;
      default: break;
    }
  }

  void type(tree t, enum tree_code code, std::string &o) {
    o += "(uid . "; put_int(o, TYPE_UID(t)); o += ')';
    field_ref(o, "name", TYPE_NAME(t));
    field_ref(o, "size", TYPE_SIZE(t));
    field_int(o, "align", TYPE_ALIGN(t));
    field_int(o, "quals", TYPE_QUALS(t));
    if (TYPE_MAIN_VARIANT(t) != t) field_ref(o, "main-variant", TYPE_MAIN_VARIANT(t));
    if (INTEGRAL_TYPE_P(t) || SCALAR_FLOAT_TYPE_P(t)) {
      field_int(o, "precision", TYPE_PRECISION(t));
      field_bool(o, "unsigned", TYPE_UNSIGNED(t));
    }
    switch (code) {
      case INTEGER_TYPE: case ENUMERAL_TYPE: case BOOLEAN_TYPE:
        field_ref(o, "min", TYPE_MIN_VALUE(t));
        field_ref(o, "max", TYPE_MAX_VALUE(t));
        break;
      default: break;
    }
    switch (code) {
      case ARRAY_TYPE:
        field_ref(o, "domain", TYPE_DOMAIN(t));
        break;
      case RECORD_TYPE: case UNION_TYPE: case QUAL_UNION_TYPE:
        field_chain(o, "fields", TYPE_FIELDS(t));
        break;
      case FUNCTION_TYPE: case METHOD_TYPE:
        field_ref(o, "arg-types", TYPE_ARG_TYPES(t));
        break;
      case ENUMERAL_TYPE:
        field_ref(o, "values", TYPE_VALUES(t));
        break;
      default: break;
    }
  }

  void constant(tree t, enum tree_code code, std::string &o) {
    switch (code) {
      case INTEGER_CST: {
        o += "(value . ";
        if (tree_fits_shwi_p(t)) put_int(o, (long long)tree_to_shwi(t));
        else if (tree_fits_uhwi_p(t)) o += std::to_string((unsigned long long)tree_to_uhwi(t));
        else {
          // wide value: little-endian HOST_WIDE_INT limbs, sign-extended
          wide_int w = wi::to_wide(t);
          o += "#s(Wide "; put_int(o, w.get_precision()); o += " (";
          for (unsigned i = 0; i < w.get_len(); i++) { if (i) o += ' '; put_int(o, (long long)w.elt(i)); }
          o += "))";
        }
        o += ')';
        break;
      }
      case REAL_CST: {
        char buf[128];
        real_to_hexadecimal(buf, TREE_REAL_CST_PTR(t), sizeof buf, 0, 1);
        field_str(o, "hex", buf);
        break;
      }
      case STRING_CST:
        field(o, "bytes"); put_bytes(o, TREE_STRING_POINTER(t), TREE_STRING_LENGTH(t)); o += ')';
        break;
      case COMPLEX_CST:
        field_ref(o, "real", TREE_REALPART(t));
        field_ref(o, "imag", TREE_IMAGPART(t));
        break;
      default:
        field_bool(o, "unknown", true);
        break;
    }
  }

  void exceptional(tree t, enum tree_code code, std::string &o) {
    switch (code) {
      case IDENTIFIER_NODE:
        field_str(o, "id", IDENTIFIER_POINTER(t));
        break;
      case TREE_LIST:
        field_ref(o, "purpose", TREE_PURPOSE(t));
        field_ref(o, "value", TREE_VALUE(t));
        field_ref(o, "chain", TREE_CHAIN(t));
        break;
      case TREE_VEC: {
        field(o, "elts"); o += '(';
        for (int i = 0; i < TREE_VEC_LENGTH(t); i++) { if (i) o += ' '; ref(o, TREE_VEC_ELT(t, i)); }
        o += "))";
        break;
      }
      case BLOCK:
        field_chain(o, "vars", BLOCK_VARS(t));
        field_chain(o, "subblocks", BLOCK_SUBBLOCKS(t));
        break;
      case STATEMENT_LIST: {
        field(o, "stmts"); o += '(';
        bool first = true;
        for (tree_stmt_iterator i = tsi_start(t); !tsi_end_p(i); tsi_next(&i)) {
          if (!first) o += ' '; first = false; ref(o, tsi_stmt(i));
        }
        o += "))";
        break;
      }
      case CONSTRUCTOR: {
        field(o, "elts"); o += '(';
        unsigned ix; tree idx, val; bool first = true;
        FOR_EACH_CONSTRUCTOR_ELT(CONSTRUCTOR_ELTS(t), ix, idx, val) {
          if (!first) o += ' '; first = false;
          o += '('; ref(o, idx); o += " . "; ref(o, val); o += ')';
        }
        o += "))";
        break;
      }
      case ERROR_MARK:
        break;
      default:
        field_bool(o, "unknown", true);
        break;
    }
  }
};

// ---------------------------------------------------------------- callbacks

void on_pre_genericize(void *gcc_data, void *) {
  tree fndecl = (tree)gcc_data;
  if (!fndecl || TREE_CODE(fndecl) != FUNCTION_DECL) return;
  Serializer s;
  int decl_id = s.visit(fndecl);
  int body_id = s.visit(DECL_SAVED_TREE(fndecl));
  std::string &o = g_fns;
  o += "  #s(Fn ";
  tree name = DECL_NAME(fndecl);
  put_string(o, name ? IDENTIFIER_POINTER(name) : "");
  o += " #s(TreeId "; put_int(o, decl_id); o += ") ";
  if (body_id < 0) o += "#f"; else { o += "#s(TreeId "; put_int(o, body_id); o += ')'; }
  o += "\n    #(";
  for (size_t i = 0; i < s.nodes.size(); i++) { o += "\n      "; o += s.nodes[i]; }
  o += "))\n";
  g_fn_count++;
}

void on_finish_unit(void *, void *) {
  FILE *f = fopen(g_out_path.c_str(), "w");
  if (!f) { fprintf(stderr, "gccast: cannot open %s\n", g_out_path.c_str()); return; }
  std::string o;
  o += ";; gccast " GCCAST_GCC_VERSION " -- GENERIC dump; read with Racket `read`\n";
  o += "#s(TU \"" GCCAST_GCC_VERSION "\" ";
  put_string(o, main_input_filename ? main_input_filename : "");
  o += " (\n";
  o += g_fns;
  o += "))\n";
  fwrite(o.data(), 1, o.size(), f);
  fclose(f);
  if (getenv("GCCAST_VERBOSE"))
    fprintf(stderr, "gccast: wrote %d function(s) to %s\n", g_fn_count, g_out_path.c_str());
}

plugin_info g_info = { "gccast " GCCAST_GCC_VERSION, "Serialize GENERIC to a Racket-readable .dat (arg: -fplugin-arg-gccast-out=PATH)" };

} // namespace

int plugin_init(struct plugin_name_args *info, struct plugin_gcc_version *version) {
  if (!plugin_default_version_check(version, &gcc_version)) {
    fprintf(stderr, "gccast: built for GCC %s, running GCC %s\n", gcc_version.basever, version->basever);
    return 1;
  }
  for (int i = 0; i < info->argc; i++)
    if (!strcmp(info->argv[i].key, "out") && info->argv[i].value) g_out_path = info->argv[i].value;
  if (g_out_path.empty())
    g_out_path = std::string(main_input_filename ? main_input_filename : "a") + ".gccast.dat";
  register_callback(info->base_name, PLUGIN_INFO, nullptr, &g_info);
  register_callback(info->base_name, PLUGIN_PRE_GENERICIZE, on_pre_genericize, nullptr);
  register_callback(info->base_name, PLUGIN_FINISH_UNIT, on_finish_unit, nullptr);
  return 0;
}
