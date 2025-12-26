package com.cutiedeng.util;

import org.objectweb.asm.*;

import java.util.*;
import java.io.*;

public class StringUtil {
  public static void toQuotedString(PrintStream out, String content) {
    if (content.contains("\"")) {
      throw new IllegalArgumentException(String.format("invalid string for quoted: '%s'", content));
    }
    out.printf("\"");
    out.printf("%s", content);
    out.printf("\"");
    out.printf(" ");
  }
  public static void toQuotedStrings(PrintStream out, ArrayList<String> cs) {
    for (String c: cs) {
      toQuotedString(out, c);
      out.printf(" ");
    }
  }
  public static void toString(PrintStream out, Object o, boolean needWrap) {
    if (o instanceof Long v) {
      out.printf("%d ", v);
    } else if (o instanceof String v) {
      StringUtil.toQuotedString(out, v);
    } else if (o instanceof Boolean v) {
      out.printf("%s ", (boolean) v ? "#t" : "#f");
    } else if (o instanceof ArrayList v) {
      if (needWrap) {
        out.printf("(");
      }
      for (Object o2: v) {
        toString(out, o2, true);
      }
      if (needWrap) {
        out.printf(") ");
      }
    } else if (o instanceof Double v) {
      out.printf("%f ", (double ) v);
    } else {
      throw new IllegalArgumentException(String.format("invalid args for %s #:type %s", o, o.getClass()));
    }
  }
}
