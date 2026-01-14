/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#include <stdio.h>
#include <stdlib.h>
#include <catala_runtime.h>

#include <Stdlib_en.h>
#include <Date_en.h>
#include <MonthYear_en.h>
#include <Period_en.h>
#include <Money_en.h>
#include <Integer_en.h>
#include <Decimal_en.h>
#include <List_en.h>

typedef struct dict {
  CATALA_INT key;
  CATALA_MONEY value;
  struct dict * rest;
} Dictionnary_ext_Dictionnary;

const Dictionnary_ext_Dictionnary* DictionnaryExt__empty () {
  return NULL;
}

const Dictionnary_ext_Dictionnary* DictionnaryExt__store
    (Dictionnary_ext_Dictionnary* dict,
     CATALA_INT key,
     CATALA_MONEY value)
{
  struct dict * head = catala_malloc(sizeof(struct dict));
  head->key = key;
  head->value = value;
  head->rest = dict;
  return head;
}

const CATALA_OPTION(CATALA_MONEY) DictionnaryExt__find
    (Dictionnary_ext_Dictionnary* dict, CATALA_INT key)
{
  while (dict) {
    if (mpz_cmp(key, dict->key)) dict = dict->rest;
    else return catala_some(dict->value);
  }
  return &catala_none;
}
