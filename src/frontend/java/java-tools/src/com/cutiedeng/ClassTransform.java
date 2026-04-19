package com.cutiedeng;

import org.objectweb.asm.*;

import java.io.*;
import java.util.*;

import com.cutiedeng.info.*;
import com.cutiedeng.util.*;

public class ClassTransform {
  private int uselessId = 0;
  public String jarPath = "lib/asm-9.9.jar";
  public String clazzName = "com.cutiedeng.ClassTransform";
  public static void init() throws Exception {
    AsmOpcodeUtil.addOpcodesToName(AsmOpcodeUtil.opcodeToName);
  }
  public static void main(String[] args) throws Throwable {
    init();
    new ClassTransform().executeClazz();
  }
  public void executeClazz() throws IOException {
    ClassReader r = new ClassReader(clazzName);
    ClassBuilder b = new ClassBuilder();
    r.accept(b, 0);
    b.clazz.toString(System.out);
  }
  public class ClassBuilder extends ClassVisitor {
    public DatumClass clazz;
    @Override
    public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
      clazz = DatumClass.create(name, superName, access, interfaces);
    }
    @Override
    public FieldVisitor visitField(int access, String name, String descriptor, String signature, Object value) {
      System.err.printf("=== field ===%n");
      System.err.printf("field: %s (%s)%n", name, descriptor);
      // type analyzer
      var tt = Type.getType(descriptor);
      System.err.printf("clazz name: %s%n", tt.getClassName());
      System.err.printf("internal name: %s%n", tt.getInternalName());
      if (signature != null) {
        System.err.printf("sig: %s%n", signature);
      } else {
        System.err.printf("!sig%n");
      }
      System.err.printf("access: 0x%x%n", access);
      System.err.printf("value: %s%n", value);
      System.err.printf("=========%n%n");
      DatumField f = DatumField.create(name, descriptor, access);
      FieldBuilder b = new FieldBuilder(); b.self = f;
      return b;
    }
    @Override
    public MethodVisitor visitMethod(int access, String name, String descriptor, String signature, String[] exceptions) {
      System.err.printf("=== method ===%n");
      System.err.printf("method: %s (%s)%n", name, descriptor);
      if (signature != null) {
        System.err.printf("sig: %s%n", signature);
      } else {
        System.err.printf("!sig%n");
      }
      System.err.printf("access: 0x%x%n", access);
      if (exceptions == null) { exceptions = new String[] {}; };
      System.err.printf("exceptions: %s%n", Arrays.asList(exceptions));
      System.err.printf("=========%n%n");
      DatumMethod m = DatumMethod.create(name, descriptor, access);
      MethodBuilder b = new MethodBuilder(); b.self = m;
      return b;
    }
    @Override
    public void visitInnerClass(String name, String outerName, String innerName, int access) {
      System.err.printf("=== inner class ===%n");
      System.err.printf("name: %s(out: %s, inner: %s)%n", name, outerName, innerName);
      System.err.printf("access: 0x%x%n", access);
      System.err.printf("============%n%n");
      DatumInnerClass cl = DatumInnerClass.create(name, outerName, innerName, access);
      clazz.innerClasses.add(cl);
    }
    @Override
    public void visitSource(String source, String debug) {
      System.err.printf("=== debug ===%n");
      System.err.printf("source: %s%n", source);
      System.err.printf("debug: %s%n", debug);
      System.err.printf("=============%n%n");
    }
    public class FieldBuilder extends FieldVisitor {
      public DatumField self;
      @Override
      public void visitEnd() {
        clazz.fields.add(self);
      }
      public FieldBuilder() {
        super(Opcodes.ASM9);
      }
    }
    public class MethodBuilder extends MethodVisitor {
      public DatumMethod self;
      @Override
      public void visitCode() {
        self.insns = new ArrayList();
        self.vars = new ArrayList();
        self.debugLineInfos = new ArrayList();
      }
      @Override
      public void visitFrame(int type, int numLocal, Object[] label, int numStack, Object[] stack) {
        System.err.printf("=== frame ===%n");
        System.err.printf("===========%n%n");
      }
      @Override
      public void visitInsn(int opcode) {
        ArrayList<Object> args = new ArrayList();
        DatumInsn i = DatumInsn.create(opcode, args);
        self.insns.add(i);
      }
      @Override
      public void visitIntInsn(int opcode, int operand) {
        ArrayList<Object> args = new ArrayList();
        args.add((Long) (long) operand);
        DatumInsn i = DatumInsn.create(opcode, args);
        self.insns.add(i);
      }
      @Override
      public void visitVarInsn(int opcode, int varIndex) {
        ArrayList<Object> args = new ArrayList();
        args.add((Long) (long) varIndex);
        DatumInsn i = DatumInsn.create(opcode, args);
        self.insns.add(i);
      }
      @Override
      public void visitTypeInsn(int opcode, String type) {
        ArrayList<Object> args = new ArrayList();
        args.add(type);
        DatumInsn i = DatumInsn.create(opcode, args);
        self.insns.add(i);
      }
      @Override
      public void visitFieldInsn(int opcode, String owner, String name, String descriptor) {
        ArrayList<Object> args = new ArrayList();
        args.add(owner);
        args.add(name);
        args.add(descriptor);
        DatumInsn i = DatumInsn.create(opcode, args);
        self.insns.add(i);
      }
      @Override
      public void visitMethodInsn(int opcode, String owner, String name, String descriptor, boolean isInterface) {
        ArrayList<Object> args = new ArrayList();
        args.add(owner);
        args.add(name);
        args.add(descriptor);
        args.add((Boolean) isInterface);
        DatumInsn i = DatumInsn.create(opcode, args);
        self.insns.add(i);
      }
      @Override
      public void visitInvokeDynamicInsn(String name, String descriptor, Handle bootstrapMethodHandle, Object... bootstrapMethodArguments) {
        System.err.printf("=== Dynamic Insn ===%n");
        System.err.printf("========%n%n");
      }
      @Override
      public void visitJumpInsn(int opcode, Label label) {
        System.err.printf("=== Jump Insn ===%n");
        System.err.printf("label: %s%n", label.toString());
        // System.err.printf("offset: %d%n", label.getOffset());
        System.err.printf("=============%n%n");
        ArrayList<Object> args = new ArrayList();
        args.add(label.toString());
        DatumInsn i = DatumInsn.create(opcode, args);
        self.insns.add(i);
      }
      @Override
      public void visitLabel(Label label) {
        ArrayList<Object> args = new ArrayList();
        args.add(label.toString());
        DatumInsn i = DatumInsn.createOpcode("CUTIEDENG-LABEL", args);
        self.insns.add(i);
      }
      void visitLdcInsnLong(long value) {
        ArrayList<Object> args = new ArrayList();
        args.add((Long) value);
        DatumInsn i = DatumInsn.createOpcode("LDC", args);
        self.insns.add(i);
      }
      void visitLdcInsnDouble(double value) {
        ArrayList<Object> args = new ArrayList();
        args.add((Double) value);
        DatumInsn i = DatumInsn.createOpcode("LDC", args);
        self.insns.add(i);
      }
      void visitLdcInsnString(String value) {
        ArrayList<Object> args = new ArrayList();
        args.add(value);
        DatumInsn i = DatumInsn.createOpcode("LDC", args);
        self.insns.add(i);
      }
      @Override
      public void visitLdcInsn(Object value) {
        System.err.printf("=== Ldc Insn ===%n");
        if (value instanceof Integer v) {
          visitLdcInsnLong((long) (int) v);
        } else if (value instanceof Long v) {
          visitLdcInsnLong((long) v);
        } else if (value instanceof Float v) {
          visitLdcInsnDouble((double) (float) v);
        } else if (value instanceof Double v) {
          visitLdcInsnDouble((double) v);
        } else if (value instanceof String v) {
          visitLdcInsnString(v);
        } else {
          // Type, Handle, ConstantDynamic
          // Type/OBJECT, ARRAY, METHOD
          System.err.printf("error: invalid operand for LDC: %s/%s%n", value.getClass(), value);
        }
        System.err.printf("==========%n%n");
      }
      @Override
      public void visitIincInsn(int varIndex, int increment) {
        ArrayList<Object> args = new ArrayList();
        args.add((Long) (long) varIndex);
        args.add((Long) (long) increment);
        DatumInsn i = DatumInsn.createOpcode("IINC", args);
        self.insns.add(i);
      }
      @Override
      public void visitTableSwitchInsn(int min, int max, Label dflt, Label... labels) {
        ArrayList<Object> args = new ArrayList();
        args.add((Long) (long) min);
        args.add((Long) (long) max);
        ArrayList<Object> sArgs = new ArrayList();
        sArgs.add(dflt.toString());
        for (Label label: labels) {
          sArgs.add(label.toString());
        }
        args.add(sArgs);
        DatumInsn i = DatumInsn.createOpcode("TABLESWITCH", args);
        self.insns.add(i);
      }
      @Override
      public void visitLookupSwitchInsn(Label dflt, int[] keys, Label[] labels) {
        ArrayList<Object> args = new ArrayList();
        args.add(dflt.toString());
        ArrayList<Object> sArgsL = new ArrayList();
        ArrayList<Object> sArgsR = new ArrayList();
        for (int k: keys) {
          sArgsL.add((Long) (long) k);
        }
        for (Label label: labels) {
          sArgsR.add(label.toString());
        }
        args.add(sArgsL);
        args.add(sArgsR);
        DatumInsn i = DatumInsn.createOpcode("LOOKUPSWITCH", args);
        self.insns.add(i);
      }
      @Override
      public void visitMultiANewArrayInsn(String descriptor, int numDimensions) {
        ArrayList<Object> args = new ArrayList();
        args.add(descriptor);
        args.add((Long) (long) numDimensions);
        DatumInsn i = DatumInsn.createOpcode("MULTIANEWARRAY", args);
        self.insns.add(i);
      }
      @Override
      public AnnotationVisitor visitInsnAnnotation(int typeRef, TypePath typePath, String descriptor, boolean visible) {
        if (!visible) return null;
        System.err.printf("=== ANNOTATION ===%n");
        System.err.printf("=========%n%n");
        return null;
      }
      @Override
      public void visitTryCatchBlock(Label start, Label end, Label handler, String type) {
        System.err.printf("=== try-catch ===%n");
        ArrayList<Object> args = new ArrayList();
        args.add(start.toString());
        args.add(end.toString());
        args.add(handler.toString());
        args.add(type);
        DatumInsn i = DatumInsn.createOpcode("TRY-CATCH-BLOCK", args);
        self.insns.add(i);
        System.err.printf("==========%n%n");
      }
      @Override
      public void visitLocalVariable(String name, String descriptor, String signature, Label start, Label end, int index) {
        DatumMethod.DatumLocalVar v = self.createLocalVar(name, descriptor, start.toString(), end.toString(), (long) index);
        self.vars.add(v);
        System.err.printf("=== local var '%s' ===%n", name);
        System.err.printf("name = %s, descriptor = %s, index: %d%n", name, descriptor, index);
        if (signature != null) {
          System.err.printf("sig: %s%n", signature);
        } else {
          System.err.printf("!sig%n");
        }
        // System.err.printf("from %s(%d) to %s(%d)%n", start, start.getOffset(), end, end.getOffset());
        System.err.printf("=========%n%n");
      }
      @Override
      public void visitLineNumber(int line, Label start) {
        System.err.printf("=== line number %d <- %s ===%n", line, start);
        DatumDebugLineInfo i = DatumDebugLineInfo.create(line, start.toString());
        self.debugLineInfos.add(i);
      }
      @Override
      public void visitMaxs(int maxStack, int maxLocals) {
        System.err.printf("=== summary ===%n");
        System.err.printf("max stack: %d%n", maxStack);
        System.err.printf("max locals: %d%n", maxLocals);
        System.err.printf("=========%n%n");
      }
      @Override
      public void visitEnd() {
        clazz.methods.add(self);
      }
      public MethodBuilder() {
        super(Opcodes.ASM9);
      }
    }
    public ClassBuilder() {
      super(Opcodes.ASM9);
    }
  }

  private void test(String x) {
    try {
      x.split(" ");
    } catch (Exception e) {
      x.split("1");
    }
  }
}
