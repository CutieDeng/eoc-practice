int ext(int);

int k1(int a)
{
  int x = 3;
  int y = x + 4;
  if (y > 5)
    return a + y;
  return a - y;
}

int k2(int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    s += 2;
  return s;
}

int k3(int a)
{
  int b = a;
  int c = b;
  return c + 0;
}

int k4(unsigned a)
{
  unsigned m = a & 6;
  if (m & 1)
    return ext(1);
  return ext(2);
}

int k5(int a, int b)
{
  int r;
  if (a > b)
    r = 10;
  else
    r = 10;
  return r + a;
}
