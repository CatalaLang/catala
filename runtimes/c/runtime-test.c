/* this is a basic unit test for the runtime. */

#include <stdio.h>
#include <runtime.h>
#include <assert.h>

void test()
{
  CATALA_MONEY dollars = catala_new_money_str ("10000");
  CATALA_DEC rate = catala_new_dec_str ("1/3");
  CATALA_DEC rate2 = catala_new_dec_str ("30/100");
  CATALA_DEC ratex = o_add_rat_rat(rate, rate2);
  CATALA_MONEY result = o_mult_mon_rat(dollars,ratex);
  assert (CATALA_TRUE == o_eq_mon_mon(result, catala_new_money_str("6333")));
  assert (CATALA_FALSE == o_eq_mon_mon(result, catala_new_money_str("6334")));
}

int main()
{
  catala_init(NULL);
  test();
  catala_free_all();
  return 0;
}
