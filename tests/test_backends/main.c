#include "simple.c"

typedef struct raw_input
{
    foo_struct foo_value;
    char use_foo_value;
} raw_input;

option_1_enum input_closure(void *closure_env, void *unit_arg)
{
    raw_input *input = (raw_input *)closure_env;
    if (input->use_foo_value)
    {
        option_1_enum out = {
            option_1_enum_some_1_cons,
            {
                some_1_cons :
                    {bar_enum_yes_cons, {yes_cons : input->foo_value}}
            }};
        return out;
    }
    else
    {
        option_1_enum out = {
            option_1_enum_none_1_cons,
            {
                none_1_cons :
                    NULL
            }};
        return out;
    }
}

int main()
{
    raw_input *raw_input = malloc(sizeof(raw_input));
    raw_input->foo_value.x_field = 1;
    raw_input->foo_value.y_field = 54;
    raw_input->use_foo_value = 1;
    if (!setjmp(catala_fatal_error_jump_buffer))
    {
        baz_in_struct input = {{input_closure, raw_input}};
        baz_struct output = baz_func(input);
        printf("Output: %f\n", output.b_field);
        free(raw_input);
        catala_free_allocated_pointers();
        return 0;
    }
    else
    {
        char *error_kind;
        switch (catala_fatal_error_raised.code)
        {
        case catala_no_value_provided:
            error_kind = "No value provided";
            break;
        case catala_conflict:
            error_kind = "Conflict between exceptions";
            break;
        case catala_crash:
            error_kind = "Crash";
            break;
        case catala_empty:
            error_kind = "Empty error not caught";
            break;
        case catala_assertion_failure:
            error_kind = "Asssertion failure";
            break;
        case catala_malloc_error:
            error_kind = "Malloc error";
        }
        printf("\033[1;31m[ERROR]\033[0m %s in file %s:%d.%d-%d.%d\n",
               error_kind,
               catala_fatal_error_raised.position.filename,
               catala_fatal_error_raised.position.start_line,
               catala_fatal_error_raised.position.start_column,
               catala_fatal_error_raised.position.end_line,
               catala_fatal_error_raised.position.end_column);
        free(raw_input);
        catala_free_allocated_pointers();
        return -1;
    }
}
