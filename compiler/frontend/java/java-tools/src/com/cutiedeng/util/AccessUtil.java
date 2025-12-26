package com.cutiedeng.util;

import org.objectweb.asm.*;

import java.util.*;

public class AccessUtil {
  public static void parseAccess(ArrayList<String> rst, int accmode) {
    if ((accmode & Opcodes.ACC_ABSTRACT) != 0) {
      rst.add("ABSTRACT");
    }
    if ((accmode & Opcodes.ACC_ANNOTATION) != 0) {
      rst.add("ANNOTATION");
    }
    if ((accmode & Opcodes.ACC_BRIDGE) != 0) {
      rst.add("BRIDGE");
    }
    if ((accmode & Opcodes.ACC_DEPRECATED) != 0) {
      rst.add("DEPRECATED");
    }
    if ((accmode & Opcodes.ACC_ENUM) != 0) {
      rst.add("ENUM");
    }
    if ((accmode & Opcodes.ACC_FINAL) != 0) {
      rst.add("FINAL");
    }
    if ((accmode & Opcodes.ACC_INTERFACE) != 0) {
      rst.add("INTERFACE");
    }
    if ((accmode & Opcodes.ACC_MANDATED) != 0) {
      rst.add("MANDATED");
    }
    if ((accmode & Opcodes.ACC_MODULE) != 0) {
      rst.add("MODULE");
    }
    if ((accmode & Opcodes.ACC_NATIVE) != 0) {
      rst.add("NATIVE");
    }
    if ((accmode & Opcodes.ACC_OPEN) != 0) {
      rst.add("OPEN");
    }
    if ((accmode & Opcodes.ACC_PRIVATE) != 0) {
      rst.add("PRIVATE");
    }
    if ((accmode & Opcodes.ACC_PROTECTED) != 0) {
      rst.add("PROTECTED");
    }
    if ((accmode & Opcodes.ACC_PUBLIC) != 0) {
      rst.add("PUBLIC");
    }
    if ((accmode & Opcodes.ACC_RECORD) != 0) {
      rst.add("RECORD");
    }
    if ((accmode & Opcodes.ACC_STATIC) != 0) {
      rst.add("STATIC");
    }
    if ((accmode & Opcodes.ACC_STATIC_PHASE) != 0) {
      rst.add("STATIC_PHASE");
    }
    if ((accmode & Opcodes.ACC_STRICT) != 0) {
      rst.add("STRICT");
    }
    if ((accmode & Opcodes.ACC_SUPER) != 0) {
      rst.add("SUPER");
    }
    if ((accmode & Opcodes.ACC_SYNCHRONIZED) != 0) {
      rst.add("SYNCHRONIZED");
    }
    if ((accmode & Opcodes.ACC_SYNTHETIC) != 0) {
      rst.add("SYNTHETIC");
    }
    if ((accmode & Opcodes.ACC_TRANSIENT) != 0) {
      rst.add("TRANSIENT");
    }
    if ((accmode & Opcodes.ACC_TRANSITIVE) != 0) {
      rst.add("TRANSITIVE");
    }
    if ((accmode & Opcodes.ACC_VARARGS) != 0) {
      rst.add("VARARGS");
    }
    if ((accmode & Opcodes.ACC_VOLATILE) != 0) {
      rst.add("VOLATILE");
    }
  }
}
