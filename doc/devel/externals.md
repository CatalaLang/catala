# How to write and use external functions

> NOTE: this is experimental and subject to change. Expect to get your hands
> dirty and everything to break every other Catala version. Only OCaml is
> supported at the moment.

## Write an interface file

I.e. declare your function in a catala file, defining the module as external:

    > Module Prorata_external external
    
    ```catala-metadata
    declaration prorata content collection money
      depends on amount content money,
                 weights content collection decimal
    ```

## Get the expected function prototype

To get the expected prototype the easiest at the moment is to write a dummy
catala implementation and compile to OCaml (removing the `external` directive):

    > Module Prorata_external

    ```catala
    declaration prorata content collection money
      depends on amount content money,
                 weights content collection decimal
      equals [amount]
    ```

```shell-session
$ clerk build _build/.../prorata_external.ml
```

(beware the `_build/`, it is required here)


## Write the OCaml implementation

Copy the obtained ml file besides the `.catala_en` file (adjusting
capitalisation to match). Edit to replace the dummy implementation by your code.
Refer to `runtimes/ocaml/runtime.mli` for what is available (especially the
`Oper` module to manipulate the types).

Keep the `register_module` at the end, but replace the hash (which should be of
the form `"CM0|XXXXXXXX|XXXXXXXX|XXXXXXXX"`) by the string `"*external*"`. This
section is needed for the Catala interpreter to find the declared values --- the
error `Failure("Could not resolve reference to Xxx")` during evaluation is a
symptom that it is missing.

## Compile and test

`Clerk` should pick up the `.ml` file automatically. Make sure you cleaned up
the dummy implementation version from step 2 (and declare as `external`) in the
`.catala_en` file and that the `.ml` file with the same name sits besides it.

Use the module as any other module (e.g. `> Using Prorata_external as M` then
`M.prorata`).

There is an example in `tests/test_modules/external_use.catala_en`.
