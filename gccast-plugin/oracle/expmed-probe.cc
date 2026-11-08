// expmed oracle (GPLv3, like the rest of gccast-plugin): a GCC plugin
// that prints, at the end of the translation unit, GCC's expmed cost
// tables (init_expmed), mult_by_coeff_cost for a set of coefficients
// (alg_hash cleared before each) and a sequence of mult_by_coeff_cost
// calls sharing alg_hash, followed by the alg_hash entries, as
// s-expressions.  The unit must contain a function: init_expmed runs
// when the first function is expanded (init_function_start).
// Built and run by oracle/expmed.sh; read by the tests of
// src/component/gimple/opt/expmed.rkt.
#include "gcc-plugin.h"
#include "plugin-version.h"
#include "tree.h"
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "expmed.h"
#include "predict.h"
#include <cstdio>
#include <cstring>
#include <vector>

int plugin_is_GPL_compatible;
static const char *out_path;

static const machine_mode modes[] = { E_QImode, E_HImode, E_SImode, E_DImode };
static const char *mname (machine_mode m)
{
  switch (m) { case E_QImode: return "QI"; case E_HImode: return "HI"; case E_SImode: return "SI"; case E_DImode: return "DI"; default: return "?"; }
}
static void clear_hash ()
{
  memset (alg_hash_entry_ptr (0), 0, sizeof (struct alg_hash_entry) * NUM_ALG_HASH_ENTRIES);
}

static void probe (void *, void *)
{
  FILE *f = fopen (out_path, "w");
  fprintf (f, "(modes");
  for (machine_mode m : modes) fprintf (f, " (\"%s\" %d)", mname (m), (int) m);
  fprintf (f, ")\n");
  for (int speed = 0; speed < 2; speed++)
    {
      fprintf (f, "(zero %d %d)\n", speed, zero_cost (speed));
      for (machine_mode m : modes)
	{
	  int bits = GET_MODE_BITSIZE (m).to_constant ();
	  fprintf (f, "(table %d \"%s\" (add %d) (neg %d) (mul %d)", speed, mname (m), add_cost (speed, m), neg_cost (speed, m), mul_cost (speed, m));
	  fprintf (f, " (shift"); for (int i = 0; i < bits; i++) fprintf (f, " %d", shift_cost (speed, m, i)); fprintf (f, ")");
	  fprintf (f, " (shiftadd"); for (int i = 0; i < bits; i++) fprintf (f, " %d", shiftadd_cost (speed, m, i)); fprintf (f, ")");
	  fprintf (f, " (shiftsub0"); for (int i = 0; i < bits; i++) fprintf (f, " %d", shiftsub0_cost (speed, m, i)); fprintf (f, ")");
	  fprintf (f, " (shiftsub1"); for (int i = 0; i < bits; i++) fprintf (f, " %d", shiftsub1_cost (speed, m, i)); fprintf (f, ")");
	  fprintf (f, " (convert");
	  for (machine_mode from : modes) fprintf (f, " (\"%s\" %d)", mname (from), convert_cost (m, from, speed));
	  fprintf (f, "))\n");
	}
    }
  std::vector<HOST_WIDE_INT> cs;
  for (HOST_WIDE_INT c = -300; c <= 300; c++) cs.push_back (c);
  size_t nsmall = cs.size ();
  for (int k = 0; k < 64; k++)
    {
      unsigned HOST_WIDE_INT p = HOST_WIDE_INT_1U << k;
      cs.push_back ((HOST_WIDE_INT) p); cs.push_back ((HOST_WIDE_INT) (p + 1)); cs.push_back ((HOST_WIDE_INT) (p - 1));
      cs.push_back ((HOST_WIDE_INT) -p); cs.push_back ((HOST_WIDE_INT) (-p + 1)); cs.push_back ((HOST_WIDE_INT) (-p - 1));
      cs.push_back ((HOST_WIDE_INT) (p * 3)); cs.push_back ((HOST_WIDE_INT) (p * 5 + 3)); cs.push_back ((HOST_WIDE_INT) (p * 7 - 1));
    }
  cs.push_back ((HOST_WIDE_INT) 0x5555555555555555ULL); cs.push_back ((HOST_WIDE_INT) 0xAAAAAAAAAAAAAAAAULL);
  cs.push_back ((HOST_WIDE_INT) 0x123456789abcdefULL); cs.push_back ((HOST_WIDE_INT) 0xfedcba9876543210ULL);
  cs.push_back ((HOST_WIDE_INT) 0x3333333333333333ULL); cs.push_back ((HOST_WIDE_INT) 0x0f0f0f0f0f0f0f0fULL);
  // (speed synth): the caller's speed and optimize_insn_for_speed_p ()
  static const int combos[3][2] = { {1, 1}, {0, 1}, {0, 0} };
  // all coefficients with (1 1), the small ones otherwise
  for (auto &cb : combos)
    for (machine_mode m : modes)
      {
	crtl->maybe_hot_insn_p = cb[1];
	fprintf (f, "(coeff %d %d \"%s\"", cb[0], cb[1], mname (m));
	size_t n = cb[0] && cb[1] ? cs.size () : nsmall;
	for (size_t k = 0; k < n; k++)
	  {
	    HOST_WIDE_INT c = cs[k];
	    clear_hash ();
	    fprintf (f, " (%lld %d)", (long long) c, mult_by_coeff_cost (c, m, cb[0]));
	  }
	fprintf (f, ")\n");
      }
  // a cached sequence: SI and DI queries, the cache kept
  crtl->maybe_hot_insn_p = true;
  clear_hash ();
  unsigned long long x = 12345;
  fprintf (f, "(sequence");
  for (int i = 0; i < 1500; i++)
    {
      x = x * 6364136223846793005ULL + 1442695040888963407ULL;
      HOST_WIDE_INT c;
      switch ((x >> 60) & 3)
	{
	case 0: c = (HOST_WIDE_INT) ((x >> 20) % 2001) - 1000; break;
	case 1: c = (HOST_WIDE_INT) (x >> 3); break;
	case 2: c = (HOST_WIDE_INT) ((x >> 30) % 70000) - 35000; break;
	default: c = ((HOST_WIDE_INT) 1 << ((x >> 10) % 63)) + (HOST_WIDE_INT) ((x >> 40) % 9) - 4; break;
	}
      machine_mode m = ((x >> 50) & 1) ? E_DImode : E_SImode;
      int speed = (x >> 52) & 1;
      crtl->maybe_hot_insn_p = ((x >> 53) & 7) != 0;
      fprintf (f, " (%lld \"%s\" %d %d %d)", (long long) c, mname (m), speed, (int) crtl->maybe_hot_insn_p, mult_by_coeff_cost (c, m, speed));
    }
  fprintf (f, ")\n(hash");
  for (int i = 0; i < NUM_ALG_HASH_ENTRIES; i++)
    {
      struct alg_hash_entry *e = alg_hash_entry_ptr (i);
      if (e->alg == alg_unknown) continue;
      fprintf (f, " (%d %llu \"%s\" %d %d %d %d)", i, (unsigned long long) e->t, mname (e->mode), (int) e->speed, (int) e->alg,
	       e->cost.cost, e->cost.latency);
    }
  fprintf (f, ")\n");
  fclose (f);
}

int plugin_init (struct plugin_name_args *info, struct plugin_gcc_version *ver)
{
  if (!plugin_default_version_check (ver, &gcc_version)) return 1;
  for (int i = 0; i < info->argc; i++)
    if (!strcmp (info->argv[i].key, "out")) out_path = info->argv[i].value;
  register_callback (info->base_name, PLUGIN_FINISH_UNIT, probe, NULL);
  return 0;
}
