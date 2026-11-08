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

/* dom3 registers the thread latch -> header -> the call g (-1) block is
   skipped ((7, 3) incoming; (3, 4) normal); thread_through_loop_header
   rejects it because the entry edge of the loop is not threaded */
void thr_loop(int n)
{
  int first = 1;
  for (int i = 0; i < n; i++)
    {
      if (first)
        g (-1);
      first = 0;
      g (i);
    }
}
