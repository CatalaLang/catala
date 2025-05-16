#include <stdio.h>
#include <stdlib.h>
#include <catala_runtime.h>

const catala_code_position pos = {__FILE__, 0, 0, 0, 0};

const CATALA_ARRAY(CATALA_MONEY) ProrataExternal__prorata
    (CATALA_MONEY amount, const CATALA_ARRAY(CATALA_MONEY) weights)
{
  int i;
  mpz_ptr remaining = catala_malloc(sizeof(__mpz_struct));
  mpz_t total_weight;
  CATALA_ARRAY(CATALA_MONEY) const prorata =
    catala_malloc(sizeof(catala_array));
  mpz_init_set(remaining, amount);
  for (mpz_init_set_si(total_weight, 0), i = 0; i < weights->size; i++)
    mpz_add(total_weight, total_weight, weights->elements[i]);
  prorata->size = weights->size;
  prorata->elements = catala_malloc(prorata->size * sizeof(void*));
  for (i = 0; i < prorata->size - 1; i++) {
    CATALA_DEC ratio = o_div_mon_mon(&pos, weights->elements[i], total_weight);
    CATALA_MONEY m = o_mult_mon_rat(amount, ratio);
    prorata->elements[i] = m;
    mpz_sub(remaining, remaining, m);
  }
  prorata->elements[prorata->size - 1] = remaining;
  return prorata;
}

const CATALA_ARRAY(CATALA_MONEY) ProrataExternal__prorata2
    (CATALA_MONEY amount, const CATALA_ARRAY(CATALA_MONEY) weights)
{
  int i;
  mpz_ptr remaining = catala_malloc(sizeof(__mpz_struct));
  mpz_t total_weight;
  CATALA_ARRAY(CATALA_MONEY) const prorata2 =
    catala_malloc(sizeof(catala_array));
  mpz_init_set(remaining, amount);
  for (mpz_init_set_si(total_weight, 0), i = 0; i < weights->size; i++)
    mpz_add(total_weight, total_weight, weights->elements[i]);
  prorata2->size = weights->size;
  prorata2->elements = catala_malloc(prorata2->size * sizeof(void*));
  for (i = 0; i < prorata2->size; i++) {
    CATALA_DEC ratio = o_div_mon_mon(&pos, weights->elements[i], total_weight);
    CATALA_MONEY m = o_mult_mon_rat(remaining, ratio);
    prorata2->elements[i] = m;
    mpz_sub(total_weight, total_weight, weights->elements[i]);
    mpz_sub(remaining, remaining, m);
  }
  return prorata2;
}

static const CATALA_ARRAY(CATALA_MONEY) closure_prorata
    (const CLOSURE_ENV env,
     CATALA_MONEY x0,
     const CATALA_ARRAY(CATALA_MONEY) x1)
{
  return ProrataExternal__prorata(x0, x1);
}

static const CATALA_ARRAY(CATALA_MONEY) closure_prorata2
    (const CLOSURE_ENV env,
     CATALA_MONEY x0,
     const CATALA_ARRAY(CATALA_MONEY) x1)
{
  return ProrataExternal__prorata2(x0, x1);
}

