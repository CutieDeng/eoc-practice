struct S { int x; int get() const { return x * 2; } };
int h(const S& s) { return s.get() + 1; }
