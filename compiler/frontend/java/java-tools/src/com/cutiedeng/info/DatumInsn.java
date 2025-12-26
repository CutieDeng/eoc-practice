package com.cutiedeng.info;

import org.objectweb.asm.*;

import java.io.*;
import java.util.*;

import com.cutiedeng.util.*;

public class DatumInsn {
  public String opname;
  public ArrayList<Object> args;
  public static DatumInsn create(int opcode, ArrayList<Object> args) {
    DatumInsn self = new DatumInsn();
    self.opname = AsmOpcodeUtil.ofOpcode(opcode).get();
    self.args = args;
    return self;
  }
  public static DatumInsn createOpcode(String opname, ArrayList<Object> args) {
    DatumInsn self = new DatumInsn();
    self.opname = opname;
    self.args = args;
    return self;
  }
  public void toString(PrintStream out) {
    out.printf("#s(Insn ");
    // opcode
    out.printf("%s ", opname);
    // args
    StringUtil.toString(out, args, true);
    out.printf(") ");
  }
}
