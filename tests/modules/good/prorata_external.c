#include <stdio.h>
#include <stdlib.h>
#include <catala_runtime.h>

/* const catala_code_position pos = {__FILE__, 0, 0, 0, 0}; */

/* const CATALA_ARRAY(CATALA_MONEY) ProrataExternal__prorata
 *     (CATALA_MONEY amount, const CATALA_ARRAY(CATALA_MONEY) weights)
 * {
 *   int i;
 *   mpz_t remaining;
 *   CATALA_INT count = catala_new_int(weights->size);
 *   CATALA_ARRAY(CATALA_MONEY) const prorata =
 *     catala_malloc(sizeof(catala_array));
 *   mpz_init_set(remaining, amount);
 *   prorata->size = weights->size;
 *   prorata->elements = catala_malloc(prorata->size * sizeof(void*));
 *   for (i = 0; i < prorata->size - 1; i++) {
 *     CATALA_MONEY m = o_div_mon_int(&pos, amount, count);
 *     prorata->elements[i] = m;
 *     mpz_sub(remaining, remaining, m);
 *   }
 *   return prorata;
 * } */

const CATALA_ARRAY(CATALA_MONEY) ProrataExternal__prorata
    (CATALA_MONEY amount, const CATALA_ARRAY(CATALA_MONEY) weights)
{
  CATALA_ARRAY(CATALA_MONEY) const prorata =
    catala_malloc(sizeof(catala_array));
  prorata->size = 0;
  printf("PRO1\n");
  return prorata;
}

const CATALA_ARRAY(CATALA_MONEY) ProrataExternal__prorata2
    (CATALA_MONEY amount, const CATALA_ARRAY(CATALA_MONEY) weights)
{
  CATALA_ARRAY(CATALA_MONEY) const prorata2 =
    catala_malloc(sizeof(catala_array));
  prorata2->size = 0;
  printf("PRO2\n");
  return prorata2;
}

static const CATALA_ARRAY(CATALA_MONEY) closure_prorata
    (const CLOSURE_ENV env,
     CATALA_MONEY x0,
     const CATALA_ARRAY(CATALA_MONEY) x1)
{
  printf("CPRO1\n");
  return ProrataExternal__prorata(x0, x1);
}

static const CATALA_ARRAY(CATALA_MONEY) closure_prorata2
    (const CLOSURE_ENV env,
     CATALA_MONEY x0,
     const CATALA_ARRAY(CATALA_MONEY) x1)
{
  printf("CPRO2\n");
  return ProrataExternal__prorata2(x0, x1);
}

