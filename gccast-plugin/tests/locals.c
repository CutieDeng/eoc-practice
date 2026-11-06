struct P { int x; int y; };
int take(int *p);
int locals(int n) {
  int arr[4];
  struct P p;
  int s = 0;
  int addr = 5;
  arr[0] = n; arr[1] = n + 1; arr[2] = arr[0] + arr[1]; arr[3] = 0;
  p.x = arr[2]; p.y = take(&addr);
  for (int i = 0; i < 4; i++) s += arr[i];
  { int inner[2]; inner[0] = s; inner[1] = p.x; s = inner[0] + inner[1]; }
  return s + p.y + addr;
}
