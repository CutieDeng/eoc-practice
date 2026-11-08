
;; Function au_two (au_two, funcdef_no=0, decl_uid=4467, cgraph_uid=1, symbol_order=0)

int au_two (int n, int * a)
{
  int v;
  int j;
  int i;
  int s;
  int _1;
  long unsigned int _2;
  long unsigned int _3;
  int * _4;
  int _5;
  int _6;
  long unsigned int _7;
  long unsigned int _8;
  int * _9;
  int _10;

  <bb 2> [local count: 118111600]:
  if (n_16(D) > 0)
    goto <bb 3>; [89.00%]
  else
    goto <bb 4>; [11.00%]

  <bb 3> [local count: 955630224]:
  # s_30 = PHI <s_23(3), 0(2)>
  # i_34 = PHI <i_24(3), 0(2)>
  _1 = i_34 * 2;
  _2 = (long unsigned int) _1;
  _3 = _2 * 4;
  _4 = a_18(D) + _3;
  _5 = *_4;
  s_23 = _5 + s_30;
  i_24 = i_34 + 1;
  if (n_16(D) > i_24)
    goto <bb 3>; [89.00%]
  else
    goto <bb 4>; [11.00%]

  <bb 4> [local count: 118111600]:
  # s_31 = PHI <s_23(3), 0(2)>
  if (n_16(D) > 0)
    goto <bb 5>; [89.00%]
  else
    goto <bb 6>; [11.00%]

  <bb 5> [local count: 955630224]:
  # s_32 = PHI <s_21(5), s_31(4)>
  # j_35 = PHI <j_22(5), 0(4)>
  _6 = j_35 * 2;
  _7 = (long unsigned int) _6;
  _8 = _7 * 4;
  _9 = a_18(D) + _8;
  _10 = *_9;
  v_20 = g (_10);
  s_21 = s_32 + v_20;
  j_22 = j_35 + 1;
  if (n_16(D) > j_22)
    goto <bb 5>; [89.00%]
  else
    goto <bb 6>; [11.00%]

  <bb 6> [local count: 118111600]:
  # s_33 = PHI <s_21(5), s_31(4)>
  return s_33;

}



;; Function au_abort (au_abort, funcdef_no=1, decl_uid=4483, cgraph_uid=2, symbol_order=1)

int au_abort (int * a, int n)
{
  int i;
  int s;
  int _1;
  long unsigned int _2;
  long unsigned int _3;
  int * _4;
  int _5;

  <bb 2> [local count: 118111600]:
  if (n_8(D) > 0)
    goto <bb 3>; [89.00%]
  else
    goto <bb 6>; [11.00%]

  <bb 3> [local count: 955630224]:
  # s_16 = PHI <s_11(5), 0(2)>
  # i_18 = PHI <i_12(5), 0(2)>
  _1 = i_18 * 2;
  _2 = (long unsigned int) _1;
  _3 = _2 * 4;
  _4 = a_10(D) + _3;
  _5 = *_4;
  if (_5 < 0)
    goto <bb 4>; [0.00%]
  else
    goto <bb 5>; [100.00%]

  <bb 4> [count: 0]:
  abort ();

  <bb 5> [local count: 955630224]:
  s_11 = _5 + s_16;
  i_12 = i_18 + 1;
  if (n_8(D) > i_12)
    goto <bb 3>; [89.00%]
  else
    goto <bb 6>; [11.00%]

  <bb 6> [local count: 118111600]:
  # s_17 = PHI <s_11(5), 0(2)>
  return s_17;

}



;; Function au_trail (au_trail, funcdef_no=2, decl_uid=4495, cgraph_uid=3, symbol_order=2)

int au_trail (struct au_s * s)
{
  int i;
  int t;
  int _1;
  int _2;
  int _3;
  int _12;

  <bb 2> [local count: 357878152]:
  _12 = s_7(D)->n;
  if (_12 > 0)
    goto <bb 3>; [66.67%]
  else
    goto <bb 4>; [33.33%]

  <bb 3> [local count: 715863672]:
  # t_13 = PHI <t_8(3), 0(2)>
  # i_15 = PHI <i_9(3), 0(2)>
  _1 = i_15 * 2;
  _2 = s_7(D)->d[_1];
  t_8 = _2 + t_13;
  i_9 = i_15 + 1;
  _3 = s_7(D)->n;
  if (_3 > i_9)
    goto <bb 3>; [66.67%]
  else
    goto <bb 4>; [33.33%]

  <bb 4> [local count: 357878152]:
  # t_14 = PHI <t_8(3), 0(2)>
  return t_14;

}



;; Function au_rec (au_rec, funcdef_no=3, decl_uid=4504, cgraph_uid=4, symbol_order=3)

int au_rec (int n)
{
  int v;
  int i;
  int s;
  int _1;

  <bb 2> [local count: 236223200]:
  if (n_6(D) > 0)
    goto <bb 3>; [78.00%]
  else
    goto <bb 4>; [22.00%]

  <bb 3> [local count: 837518624]:
  # s_13 = PHI <s_8(3), 0(2)>
  # i_15 = PHI <i_9(3), 0(2)>
  _1 = i_15 + -1;
  v_7 = au_rec (_1);
  s_8 = s_13 + v_7;
  i_9 = i_15 + 1;
  if (n_6(D) > i_9)
    goto <bb 3>; [78.00%]
  else
    goto <bb 4>; [22.00%]

  <bb 4> [local count: 236223200]:
  # s_14 = PHI <s_8(3), 0(2)>
  return s_14;

}



;; Function au_unused (au_unused, funcdef_no=4, decl_uid=4514, cgraph_uid=5, symbol_order=4)

void au_unused (int x)
{
  <bb 2> [local count: 1073741824]:
  g (x_2(D));
  return;

}


