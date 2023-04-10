
<a href="/runtimes/python/catala/src/catala/concolic.py#L675"><img align="right" style="float:right;" src="https://img.shields.io/badge/-source-cccccc?style=flat-square"></a>

# <kbd>function</kbd> `concolic_run`

```python
concolic_run(
    func: Callable[[ConcolicInput], Any],
    vars: List[ConcolicVar],
    constraints: List[Callable[[ConcolicInput], ConcolicBoolean]] = None,
    preferences: List[Callable[[ConcolicInput], ConcolicBoolean]] = None,
    logging: ConcolicLogLevel = <ConcolicLogLevel.NONE: 0>,
    currency_locale: str = None
) → Tuple[int, bool]
```

Performs systematic testing of a given runnable and returns the number of paths + whether a bug was found.



**Args:**

 - <b>`func`</b>:  A runnable that expects a `ConcolicInput` parameter. A `ConcolicInput` is an object whose  fields correspond to the names of `vars`. The runnable is the test method which will be   executed with concolic inputs, once per execution path. This function can return any value;  the returned value will be printed on screen when using log level of `INFO` or above.
 - <b>`vars`</b>:  A list of input variables whose values will be generated automatically using concolic execution.  Each `ConcolicVar` contains a `name` string, a `type` string (one of [`Integer`, `Money`, `Decimal`, `Date`]),  and an `initial` value.
 - <b>`constraints`</b>:  A collection of constraints that must be satisfied by the input variables. Each constraint  is represented as a function (typically a lambda) whose parameter is a `ConcolicInput` object having   each field be a Z3 variable corresponding to input variables.
 - <b>`preferences`</b>:  Similar to `constraints`, but this collection represents constraints that we would like  to satisfy on a best-effort basis. Typically, these include constraints for making values "look nice",  such as asking money values to be a nice round number or expecting dates to be within some range close  to the present day.
 - <b>`logging`</b>:  A `ConcolicLogLevel` that determines how much output is printed to stdout during the concolic testing  process. Options (in increasing order) are `ConcolicLogLevel.NONE`, `.ERROR`, `.INFO`, `.VERBOSE`, `.DEBUG`.
 - <b>`currency_locale`</b>:  An optional config string representing the locale for formatting currency values when printing.   Examples include `en_US` or `fr_FR`. If not specified, money values are printed in cents with no thousands   separator.



**Returns:**
 A tuple whose first component is the number of distinct execution paths (and corresponding test inputs) exercised by the concolic execution engine, and the second component is a boolean which is `True` if and only if  any test failed (for example, because of an `AssertionError` or a `ConflictError`).

