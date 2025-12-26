package com.cutiedeng.info;

import org.objectweb.asm.*;

import java.io.*;
import java.util.*;

import com.cutiedeng.util.*;

public class DatumAnnotation {
  public String name;
  public String descriptor;
  public static DatumAnnotation create(String name, String descriptor) {
    DatumAnnotation self = new DatumAnnotation();
    self.name = name;
    self.descriptor = descriptor;
    return self;
  }
  public void toString(PrintStream out) {
    out.printf("#s(Annotation ");
    // name
    StringUtil.toQuotedString(out, name);
    // descriptor
    StringUtil.toQuotedString(out, descriptor);
    out.printf(") ");
  }
}
