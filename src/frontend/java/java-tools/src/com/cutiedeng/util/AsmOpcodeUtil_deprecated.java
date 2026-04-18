package com.cutiedeng.util;

import org.objectweb.asm.*;

import java.util.*;
import java.io.*;
import java.lang.reflect.*;

public class AsmOpcodeUtil_deprecated {
  public static boolean emitAsmOpcodeMethodInsn(PrintStream out, int opcode) {
    String op;
    if (opcode == Opcodes.INVOKEVIRTUAL) {
      op = "INVOKEVIRTUAL"; 
    } else if (opcode == Opcodes.INVOKESPECIAL) {
      op = "INVOKESPECIAL";
    } else if (opcode == Opcodes.INVOKESTATIC) {
      op = "INVOKESTATIC";
    } else if (opcode == Opcodes.INVOKEINTERFACE) {
      op = "INVOKEINTERFACE";
    } else {
      return false;
    }
    out.printf("%s ", op);
    return true;
  }
  public static boolean emitAsmOpcodeJumpInsn(PrintStream out, int opcode) {
    String op;
    if (opcode == Opcodes.IFEQ) {
      op = "IFEQ"; 
    } else if (opcode == Opcodes.IFNE) {
      op = "IFNE";
    } else if (opcode == Opcodes.IFLT) {
      op = "IFLT";
    } else if (opcode == Opcodes.IFGE) {
      op = "IFGE";
    } else if (opcode == Opcodes.IFGT) {
      op = "IFGT";
    } else if (opcode == Opcodes.IFLE) {
      op = "IFLE";
    } else if (opcode == Opcodes.IF_ICMPEQ) {
      op = "IF_ICMPEQ";
    } else if (opcode == Opcodes.IF_ICMPNE) {
      op = "IF_ICMPNE";
    } else if (opcode == Opcodes.IF_ICMPLT) {
      op = "IF_ICMPLT";
    } else if (opcode == Opcodes.IF_ICMPGE) {
      op = "IF_ICMPGE";
    } else if (opcode == Opcodes.IF_ICMPGT) {
      op = "IF_ICMPGT";
    } else if (opcode == Opcodes.IF_ICMPLE) {
      op = "IF_ICMPLE";
    } else if (opcode == Opcodes.IF_ACMPEQ) {
      op = "IF_ACMPEQ";
    } else if (opcode == Opcodes.IF_ACMPNE) {
      op = "IF_ACMPNE";
    } else if (opcode == Opcodes.GOTO) {
      op = "GOTO";
    } else if (opcode == Opcodes.JSR) {
      op = "JSR";
    } else if (opcode == Opcodes.IFNULL) {
      op = "IFNULL";
    } else if (opcode == Opcodes.IFNONNULL) {
      op = "IFNONNULL";
    } else {
      return false;
    }
    out.printf("%s ", op);
    return true;
  }
  public static boolean emitAsmOpcodeComputeInsn(PrintStream out, int opcode) {
    String op;
    if (opcode == Opcodes.I2B) {
      op = "I2B"; 
    } else if (opcode == Opcodes.I2C) {
      op = "I2C";
    } else if (opcode == Opcodes.I2D) {
      op = "I2D";
    } else if (opcode == Opcodes.I2F) {
      op = "I2F";
    } else if (opcode == Opcodes.I2L) {
      op = "I2L";
    } else if (opcode == Opcodes.I2S) {
      op = "I2S";
    } else if (opcode == Opcodes.IADD) {
      op = "IADD";
    } else if (opcode == Opcodes.IALOAD) {
      op = "IALOAD";
    } else if (opcode == Opcodes.IAND) {
      op = "IAND";
    } else if (opcode == Opcodes.IASTORE) {
      op = "IASTORE";
    } else if (opcode == Opcodes.ICONST_0) {
      op = "ICONST_0";
    } else if (opcode == Opcodes.ICONST_1) {
      op = "ICONST_1";
    } else if (opcode == Opcodes.ICONST_2) {
      op = "ICONST_2";
    } else if (opcode == Opcodes.ICONST_3) {
      op = "ICONST_3";
    } else if (opcode == Opcodes.ICONST_4) {
      op = "ICONST_4";
    } else if (opcode == Opcodes.ICONST_5) {
      op = "ICONST_5";
    } else if (opcode == Opcodes.ICONST_M1) {
      op = "ICONST_M1";
    } else if (opcode == Opcodes.IDIV) {
      op = "IDIV";
    } else {
      return false;
    }
    out.printf("%s ", op);
    return true;
  }
  public static boolean emitAsmOpcodeCompute2Insn(PrintStream out, int opcode) {
    String op;
    if (opcode == Opcodes.IINC) {
      op = "IINC"; 
    } else if (opcode == Opcodes.ILOAD) {
      op = "ILOAD";
    } else if (opcode == Opcodes.IMUL) {
      op = "IMUL";
    } else if (opcode == Opcodes.INEG) {
      op = "INEG";
    } else if (opcode == Opcodes.INSTANCEOF) {
      op = "INSTANCEOF";
    } else if (opcode == Opcodes.IOR) {
      op = "IOR";
    } else if (opcode == Opcodes.IREM) {
      op = "IREM";
    } else if (opcode == Opcodes.ISHL) {
      op = "ISHL";
    } else if (opcode == Opcodes.ISHR) {
      op = "ISHR";
    } else if (opcode == Opcodes.ISTORE) {
      op = "ISTORE";
    } else if (opcode == Opcodes.ISUB) {
      op = "ISUB";
    } else if (opcode == Opcodes.IUSHR) {
      op = "IUSHR";
    } else if (opcode == Opcodes.IXOR) {
      op = "IXOR";
    } else {
      return false;
    }
    out.printf("%s ", op);
    return true;
  }
}
