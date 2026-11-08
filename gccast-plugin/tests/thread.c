int g(int);

int thr(int a, int b)
{
  int x;
  if (a == 1)
    x = b + 1;
  else if (a == 2)
    x = b * 3;
  else
    x = b - 7;
  int y = x * x + b;
  if (a == 1)
    return g(y);
  if (a == 2)
    return g(y + 5);
  return y;
}
