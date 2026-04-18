package com.cutiedeng.info;

import org.objectweb.asm.*;

import java.io.*;
import java.util.*;

import com.cutiedeng.util.*;

public class DatumInnerClass {
  public String name;
  public String outer;
  public String inner;
  public ArrayList<String> access = new ArrayList();
  public static DatumInnerClass create(String name, String outer, String inner, int access) {
    DatumInnerClass self = new DatumInnerClass();
    self.name = name;
    self.outer = outer;
    self.inner = inner;
    AccessUtil.parseAccess(self.access, access);
    return self;
  }
  public void toString(PrintStream out) {
    out.printf("#s(InnerClass ");
    // name
    StringUtil.toQuotedString(out, name);
    // descriptor
    StringUtil.toQuotedString(out, outer);
    // descriptor
    StringUtil.toQuotedString(out, inner);
    // access
    out.printf("(");
    for (String a: access) {
      out.printf("%s ", a);
    }
    out.printf(") ");
    out.printf(") ");
  }
}
