package com.cutiedeng.info;

import org.objectweb.asm.*;

import java.io.*;
import java.util.*;

import com.cutiedeng.util.*;

public class DatumDebugLineInfo {
  public int lineNumber;
  public String label;
  public static DatumDebugLineInfo create(int l, String la) {
    DatumDebugLineInfo self = new DatumDebugLineInfo();
    self.lineNumber = l;
    self.label = la;
    return self;
  }
  public void toString(PrintStream out) {
    out.printf("#s(Line-Info ");
    // line number
    out.printf("%d ", lineNumber);
    // label
    StringUtil.toQuotedString(out, label);
    out.printf(") ");
  }
}
