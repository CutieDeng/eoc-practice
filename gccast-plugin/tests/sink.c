int h(int);

/* sink1 merges the two stores to *p into the join block
   (sink_common_stores_to_bb): one store of a PHI of the stored values */
void st_common(int *p, int a, int b)
{
  if (a > 0)
    {
      h (a);
      *p = b + 1;
    }
  else
    {
      h (b);
      *p = a;
    }
}
