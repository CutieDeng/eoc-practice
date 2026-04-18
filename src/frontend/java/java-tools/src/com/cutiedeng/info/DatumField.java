package com.cutiedeng.info;

import org.objectweb.asm.*;

import java.io.*;
import java.util.*;

import com.cutiedeng.util.*;

public class DatumField {
  public String name;
  public String descriptor;
  public ArrayList<String> access = new ArrayList();
  public static DatumField create(String name, String descriptor, int access) {
    DatumField self = new DatumField();
    self.name = name;
    self.descriptor = descriptor;
    AccessUtil.parseAccess(self.access, access);
    return self;
  }
  public void toString(PrintStream out) {
    out.printf("#s(Field ");
    // name
    StringUtil.toQuotedString(out, name);
    // descriptor
    StringUtil.toQuotedString(out, descriptor);
    // access
    out.printf("(");
    for (String a: access) {
      out.printf("%s ", a);
    }
    out.printf(") ");
    // static/init-value
    out.printf("#f ");
    out.printf(") ");
  }
}
