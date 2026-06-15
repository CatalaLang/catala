/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#ifndef __TEXT_DICT_H__
#define __TEXT_DICT_H__

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
  Text_Text key;
  ModDef__Mod_def value;
  const struct Text_dict_dict * rest;
} Text_dict_Text_dict;

/* This should be left unchanged */
const catala_type catala_type__Text_dict_Text_dict();

const Text_dict_Text_dict* TextDict__empty ();

const Text_dict_Text_dict* TextDict__store
    (const Text_dict_Text_dict* dict,
     const Text_Text* key,
     const ModDef__Mod_def* value);

const CATALA_OPTION(ModDef__Mod_def*) TextDict__find
    (const Text_dict_Text_dict* dict, const Text_Text* key);

#endif /* __TEXT_DICT_H__ */
