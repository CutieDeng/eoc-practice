int ext(int, int);
static int twice(int v) { return v * 2; }
int use(int a, int b, int *p) {
  int r = 0;
  if (a > 0 && b < 10) r = ext(a, b);
  else if (a == 0 || p == 0) r = twice(b);
  r += (a > b) ? a : b;
  *p = r++;
  p[1] = -r;
  return r + twice(*p);
}
