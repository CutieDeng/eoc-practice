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

int h(int);

/* dom2 threads 2 -> 4 through the joiner block 4 (its condition stays)
   into the empty block 5, whose condition x == 1 is known on that path */
int thr_join(int a, int b)
{
  int x;
  if (a > 0)
    x = 1;
  else
    x = h(b);
  int y = b * b + a;
  int z = y ^ b;
  if (b > 10)
    {
      if (x == 1)
        return h(z);
      return z + 3;
    }
  return y;
}
