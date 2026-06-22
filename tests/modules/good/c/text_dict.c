/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <catala_runtime.h>

#include <Stdlib_en.h>
#include <Date_en.h>
#include <List_en.h>
#include <Duration_en.h>
#include <MonthYear_en.h>
#include <Period_en.h>
#include <Money_en.h>
#include <Integer_en.h>
#include <Decimal_en.h>
#include <Text.h>
#include <Mod_def.h>

typedef struct Text_dict_dict {
  const Text_Text * key;
  const ModDef__Mod_def * value;
  const struct Text_dict_dict * rest;
} Text_dict_Text_dict;

const Text_dict_Text_dict * TextDict__empty () {
  return NULL;
}

const Text_dict_Text_dict* TextDict__store
    (const Text_dict_Text_dict * dict,
     const Text_Text * key,
     const ModDef__Mod_def * value)
{
  Text_dict_Text_dict * head = catala_malloc(sizeof(Text_dict_Text_dict));
  head->key = key;
  head->value = value;
  head->rest = dict;
  return head;
}

const CATALA_OPTION(const ModDef__Mod_def *) TextDict__find
    (const Text_dict_Text_dict * dict, const Text_Text * key)
{
  while (dict) {
    if (catala_equal(catala_type__Text_Text(), NULL, key, dict->key))
      return catala_some((void*)(dict->value));
    dict = dict->rest;
  }
  return &catala_none;
}
