#include <catala_runtime.h>
#include <gmp.h>

CATALA_MONEY MoneyInternal__round_to_decimal(CATALA_MONEY variable, CATALA_INT nth_decimal)
{
  signed long int nth_int = mpz_get_si(nth_decimal);
  if (nth_int >= 2) {
    return variable;
  } else {
    CATALA_INT ten = catala_new_int(10);
    if (nth_int == 1) {
      CATALA_MONEY m = o_round_mon(o_mult_mon_int(variable, ten));
      return o_div_mon_int(NULL, m, ten);
    } else {
      mpz_ptr pow_ten = catala_malloc(sizeof(__mpz_struct));
      mpz_pow_ui(pow_ten, ten, -nth_int);
      return o_mult_mon_int(o_round_mon(o_div_mon_int(NULL, variable, pow_ten)), pow_ten);
    }
  }
}
