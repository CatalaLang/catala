#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

typedef enum catala_fatal_error_code
{
    catala_no_value_provided,
    catala_conflict,
    catala_crash,
    catala_empty,
} catala_fatal_error_code;

typedef struct catala_code_position
{
    char *filename;
    unsigned int start_line;
    unsigned int start_column;
    unsigned int end_line;
    unsigned int end_column;
} catala_code_position;

typedef struct catala_fatal_error
{
    catala_code_position position;
    catala_fatal_error_code code;
} catala_fatal_error;

catala_fatal_error catala_fatal_error_raised;

jmp_buf catala_fatal_error_jump_buffer;
