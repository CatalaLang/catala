#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

typedef enum catala_fatal_error_code
{
  catala_assertion_failed,
  catala_no_value,
  catala_conflict,
  catala_division_by_zero,
  catala_not_same_length,
  catala_uncomparable_durations,
  catala_indivisible_durations,
  catala_malloc_error,
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

typedef struct pointer_list pointer_list;
struct pointer_list
{
    void *current;
    pointer_list *next;
};

pointer_list *catala_allocated_pointers_list = NULL;

void *
catala_malloc(size_t malloc_size)
{
    void *output = malloc(malloc_size);
    if (output == NULL)
    {
        catala_fatal_error_raised.code = catala_malloc_error;
        longjmp(catala_fatal_error_jump_buffer, 0);
    }
    else
    {
        pointer_list *old_pointer_list = catala_allocated_pointers_list;
        catala_allocated_pointers_list = malloc(sizeof(pointer_list));
        if (catala_allocated_pointers_list == NULL)
        {
            catala_fatal_error_raised.code = catala_malloc_error;
            longjmp(catala_fatal_error_jump_buffer, 0);
        }
        else
        {
            catala_allocated_pointers_list->current = output;
            catala_allocated_pointers_list->next = old_pointer_list;
        }
        return output;
    }
}

void catala_free_allocated_pointers()
{
    while (catala_allocated_pointers_list != NULL)
    {
        free(catala_allocated_pointers_list->current);
        pointer_list *next = catala_allocated_pointers_list->next;
        free(catala_allocated_pointers_list);
        catala_allocated_pointers_list = next;
    }
}
