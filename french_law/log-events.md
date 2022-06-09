# Mechanics of log events

## Some invariants

Log events respect the following invariants:

1. _The number of log events is independent of the inputs._
> Note: seems false.
2. _Function calls always have one input and one ouput._
2. _2 <= `logEvent.information.length` <= 3_

## Structure

In the following, _**raw** log events_ will refer to the array of logs built
during the program execution -- the result of `retrieveLog` and the result of
the `-t` flag for the `Interpret` backend. In contrast, of the _**structured**
log events_ which are built from the _raw_ ones in order to reflect the
semantic in the log structure itself.

### Raw log events

For raw log events, there is four types of event:

* `Decision taken` (token: `POS`) (source code position)
* `Variable definition` (token: `VAR_DEF`)
* `Begin call` (token: `BEG`)
* `End call` (token: `END`)

```
                                                      [sub-scope call]
                                                             |
                                                        __________
event.information = [ "Scope name", "attribute name", ("Scope name" | "attribute name") ]
                                                                       ______________
                                                                              |
                                                        [sub-scope input var definition]
```

### Structured log events

```
<structured_events> := <events>+

<events> := <fun_call>
          | <subscope_call>
          | <var_def>
          | <var_def_with_fun>

<fun_call> :=
    <fun_call_beg>
        VAR_DEF                             (function input)
        <events>*
        (<var_def> | <var_def_with_fun>)    (function output)
    END

<var_def_with_fun> :=
       /-- POS
pos of |   <fun_call>+      (function calls needed to compute the variable value)
       \-> VAR_DEF

<subscope_call> :=
    <sub_var_def>*          (sub-scope attributes def)
    <sub_call_beg>
        <events>+
    END


<var_def> := POS VAR_DEF (when VAR_DEF.information.length = 2)

<sub_var_def> := POS VAR_DEF (when VAR_DEF.information.length = 3)

<fun_call_beg> := BEG (when BEG.information.length = 2)

<sub_call_beg> := BEG (when BEG.information.length = 3)
```
