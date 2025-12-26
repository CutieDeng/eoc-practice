package com.cutiedeng.util;

import org.objectweb.asm.*;

import java.util.*;
import java.io.*;
import java.lang.reflect.*;

public class AsmOpcodeUtil {
  public static boolean emitAsmOpcode(PrintStream out, int opcode) {
    Optional<String> op = ofOpcode(opcode);
    if (op.isPresent()) {
      out.printf("%s ", op.get());
      return true;
    }
    return false;
  }
  public static Optional<String> ofOpcode(int opcode) {
    return Optional.ofNullable(opcodeToName.get((Integer) opcode));
  }
  public static void addOpcodesToName(HashMap<Integer, String> m) throws SecurityException, IllegalAccessException{
    for (Field f: Opcodes.class.getFields()) {
      Class<?> t = f.getType();
      if (t != int.class) {
        continue;
      }
      var mo = f.getModifiers();
      if (!Modifier.isStatic(mo)) {
        continue;
      }
      String n = f.getName();
      if (n.startsWith("ACC_")) {
        continue;
      }
      if (n.startsWith("ASM")) {
        continue;
      }
      if (n.startsWith("V")) {
        continue;
      }
      if (n.startsWith("SOURCE_")) {
        continue;
      }
      m.put((Integer) f.getInt(null), n);
    }
  } 
  public static HashMap<Integer, String> opcodeToName = new HashMap();
}
