#include <catala_runtime.h>
#include <gmp.h>

CATALA_DEC DecimalInternal__round_to_decimal(CATALA_DEC variable, CATALA_INT nth_decimal)
{
  mpq_ptr result;
  CATALA_DEC round;
  mpq_ptr pow_ten_q;
  mpz_ptr exp;
  int cmp = mpz_sgn(nth_decimal);

  if (cmp == 0)
    return o_round_rat(variable);

  result = catala_malloc(sizeof(__mpq_struct));
  mpq_init(result);
  exp = catala_malloc(sizeof(__mpz_struct));
  mpz_abs(exp, nth_decimal);
  pow_ten_q = catala_malloc(sizeof(__mpq_struct));
  mpq_init(pow_ten_q);
  mpz_pow_ui(mpq_numref(pow_ten_q), catala_new_int(10), mpz_get_ui(exp));

  if (cmp > 0) {
    mpq_mul(result, variable, pow_ten_q);
    round = o_round_rat(result);
    mpq_div(result, round, pow_ten_q);
  } else {
    mpq_div(result, variable, pow_ten_q);
    round = o_round_rat(result);
    mpq_mul(result, round, pow_ten_q);
  }

  mpq_canonicalize(result);
  return result;
}
