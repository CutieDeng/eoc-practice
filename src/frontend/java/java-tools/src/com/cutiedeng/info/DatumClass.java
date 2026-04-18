package com.cutiedeng.info;

import org.objectweb.asm.*;

import java.io.*;
import java.util.*;

import com.cutiedeng.util.*;
import com.cutiedeng.info.*;

public class DatumClass {
  public String name;
  public Optional<String> superClazzName;
  public ArrayList<String> access = new ArrayList();
  public ArrayList<String> interfaces = new ArrayList();
  public ArrayList<DatumField> fields = new ArrayList();
  public ArrayList<DatumMethod> methods = new ArrayList();
  public ArrayList<DatumAnnotation> annotations = new ArrayList();
  public ArrayList<DatumInnerClass> innerClasses = new ArrayList();
  public static DatumClass create(String name, String superClazzName, int access, String[] interfaces) {
    DatumClass self = new DatumClass();
    self.name = name;
    self.superClazzName = Optional.of(superClazzName);
    AccessUtil.parseAccess(self.access, access);
    self.interfaces = new ArrayList(Arrays.asList(interfaces));
    return self;
  }
  public void toString(PrintStream out) {
    out.printf("#s(Class ");

    StringUtil.toQuotedString(out, name);

    out.printf("(");
    for (String a: access) {
      out.printf("%s ", a);
    }
    out.printf(") ");

    out.printf("(");
    for (DatumField f: fields) {
      f.toString(out);
    }
    out.printf(") ");

    out.printf("(");
    for (DatumMethod m: methods) {
      m.toString(out);
    }
    out.printf(") ");

    out.printf("(");
    for (DatumAnnotation a: annotations) {
      a.toString(out);
    }
    out.printf(") ");

    out.printf("(");
    for (DatumInnerClass i: innerClasses) {
      i.toString(out);
    }
    out.printf(") ");

    out.printf(") ");
  }
}
