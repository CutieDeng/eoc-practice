struct P { int x; int y; char tag; };
static const char *msg = "hi\n";
static const double half = 0.5;
unsigned long sum(const struct P *ps, int n) {
  unsigned long acc = 0;
  while (n-- > 0) { acc += ps[n].x * 2 + ps[n].y; if (ps[n].tag == 'z') break; }
  switch (n) { case 0: acc++; break; case 1: acc += 2; /* fallthrough */ default: acc += 3; }
  return acc + (unsigned long)msg[0] + (unsigned long)(half * n);
}
