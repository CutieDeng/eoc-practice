/* Source audit paths: loop walk orders, bound triggers, a cold exit,
   trailing arrays, recursion in a loop, an unused call result.  Arrays
   are read with stride 2: an access whose step is its size makes
   ivopts add autoincrement candidates (not ported).  */
void abort (void);
int g (int);

/* two sibling loops: loops_list preorder / LI_FROM_INNERMOST visit
   loop 2 before loop 1 */
int au_two (int n, int *a)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    s += a[2 * i];
  for (int j = 0; j < n; j++)
    {
      int v = g (a[2 * j]);
      s += v;
    }
  return s;
}

/* an exit to a noreturn call: single_likely_exit, determine_unlikely_bbs */
int au_abort (int *a, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    {
      if (a[2 * i] < 0)
        abort ();
      s += a[2 * i];
    }
  return s;
}

/* a trailing array of a structure behind a pointer: no upper bound */
struct au_s { int n; int d[4]; };
int au_trail (struct au_s *s)
{
  int t = 0;
  for (int i = 0; i < s->n; i++)
    t += s->d[2 * i];
  return t;
}

/* a recursive call inside a loop */
int au_rec (int n)
{
  int s = 0;
  for (int i = 0; i < n; i++)
    {
      int v = au_rec (i - 1);
      s += v;
    }
  return s;
}

/* an unused call result */
void au_unused (int x)
{
  int r = g (x);
  (void) r;
}
