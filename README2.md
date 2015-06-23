
Possible variable states:

- `ref` The variable is bound as a reference to another variable. In terms of
  the PHP memory model, this means the VSlot points to a VStore possibly
  shared with other VSlots. When reading a variable in the `ref` state, the
  result is `mixed`. Any value can be written to a variable in state `ref`.
- `undefined` The variable is not defined.
- `value`
    - `int`
    - `string`
    - `float`
    - `true`
    - `false`
    - `null`
    - `object` Any object.
        - `<class>` An instance of a specified class.
        - `function(X, Y...):W` A closure which accepts the given parameters and
          return value. The parameters/return value can be any variable state,
          including `ref` to indicate a parameter is taken by reference or a
          value is returned by reference. `undefined` for the return value
          indicates a `void` function.
    - `resource`
    - `T[]` An array of type T. T is any variable state, including `ref`.
      Including `undefined` in T is redundany, since an array can't possibly
      cover all possible keys and therefore some must be `undefined`. `array`
      is an alias for `mixed[]`, which notably excludes `ref`. You cannot pass
      an array containing references into `array` (`mixed[]`) since an array
      containing references is a different beast to an array containing mixed
      values. 

A program state contains:

1. An array of possible return values, starting as `undefined`. If `undefined`
   is present amongst other return values, this means the function sometimes
   returns a value and sometimes doesn't.
4. An array of possibly thrown exceptions. These are only strings, no
   `undefined`. Only meaningful if the `runstate` is `throw`.
2. A class name for `$this`, if any. (Initialised once and unchanged.)
3. A map from variable names to an array of variable states. When acting on a
   variable, all of its possible states must be considered.
5. A `runstate`, indicating whether the program has broken/continued a loop,
   thrown an exception, returned, or is simply waiting for the next statement,
   etc.
6. A boolean indicating whether to return by-reference. (Initialised once and
   unchanged.)

Since a single program state contains multiple possible variable states for
each variable, and multiple possible return values, and multiple possibly
thrown exceptions, a single program state in fact represents multiple _real_
program states (in the sense of the PHP runtime). The only thing a program
state contains only _one of_ is the `runstate`, and therefore multiple
programstates with the same `runstate` can be merged together (merging
together the state of all variables, the value of return values and the
possibly thrown exceptions).

Possible program `runstate` values:

- `next` The program is ready to execute the next statement/instruction. This
  is normal operation. Any program state with a `runstate` other than `next`
  is ignored while stepping through the program.
- `throw` The program has thrown an exception, waiting to be handled by the
  next `catch` block.
- `exit` The program has exited abnormally, eg via `exit;`.
- `return` The program has returned.
- `break N` The program has requested to break N loops.
- `continue N` The program has requested to continue N loops.
- `goto L` The program has requested to goto the specified label. _GOTOs are
  ignored by static analysis._

Static analysis maintains multiple possible program states as it steps through
the program. Only states in `next` state are passed through each
statement/expression, possibly splitting into multiple possible states.

Here is a description of how the different control structures operate on
program state.

# if/else

The program state splits in two, one for the `if` branch and one for the
`else`. For the `if` branch, the `if` expression is evaluated and the
expression is passed `true` specifying that it can assume that it will
evaluate to true. Depending on the expression, this will change the program
state according to assumptions in the expression.

For example, the expression `$foo instanceof Bar`, when evaluated with `true`
as its known result, will change the `$foo` variable to be of type class
`Bar`. When evaluated with `false` as its known result, it will remove class
`Bar` from the list of possible states for the `$foo` variable, if it is
present.

The following expressions will modify the program state when placed in an `if`
block:

- `$foo instanceof Bar` The types of `$foo` are filtered to only include
  instances of `Bar`. Any object/class types for variable `$foo` of which
  `Bar` is a subclass (including `object`), are replaced with simply `Bar`.
- `$foo === true` `$foo` is filtered to only include `true`.
- `$foo === false` `$foo` is filtered to only include `false`.
- `$foo === null` `$foo` is filtered to only include `null`.
- `is_string($foo)` `$foo` is filtered to only include `string`.
- `is_null($foo)` `$foo` is filtered to only include `null`.
- `is_int($foo)` `$foo` is filtered to only include `int`.
- `is_object($foo)` `$foo` is filtered to only include `object`.
- `is_float($foo)` `$foo` is filtered to only include `float`.
- `is_bool($foo)` `$foo` is filtered to only include `true` and `false`.
- `is_array($foo)` `$foo` is filtered to only include array types.
- `is_resource($foo)` `$foo` is filtered to only include `resource`.
- `!...` The inner expression is applied to the program state with the
  opposite effect, ie as the expression for an `if` block it behaves as though
  it was the `else` block.
- `... && ...` The program state is filtered through the expression on the
  left and then the expression on the right.

For the `else` branch, the same is done except the applicable type is
_removed_ from the list, if present (since we definitely know that that's not
what the variable is going to be).

Note that after filtering the types may result in the variable having an empty
list of possible states. This represents an impossible program state and the
code run in this state is considered dead.

Note also that set of states for variable `$foo` may include `ref`, which
cannot be filtered out. A variable in state `ref` remains in state `ref` until
it is `unset`.

Chained `if`/`else if`/`else` branches are considered as nested `if`/`else`
statements. `if` blocks lacking an `else` block are considered to have an
empty `else` block. 

After the program states have passed through the `if` and `else` blocks, they
are merged together. Note that program states with different `runstate` values
(eg if the `if` branch caused a `return`) cannot be merged together and remain
seperate.

# while

A `while` loop is handled the exact same way as an `if` block with an empty
`else` block, ie, the program is split in two, one assuming the expression is
`true` the other assuming the expression is `false`. However, for the `true`
case of the expression, if the resulting program state is not _included_ in (ie
it is not a subset of) the program state when the loop was first entered, then
it is _added_ to the initial program state and the loop is run again, and the
process repeats. The static analysis of a real while loop dealing with values
is implemented as a while loop dealing in types. ;)

In terms of data flow analysis, this amounts to calculating the `fixed-point`
of the control flow, and is intended to handle situations such as these:

```php
$a = 1;
$b = 's';
$c = true;
while (blah()) {
    $a = $b;
    $b = $c;
}
```

Before the run of the loop, `$a` is `int`, after one run it is `string` which
is merged with the initial state to make `int|string` and after a second run
it is `true`, which is merged again to make `int|string|true`, and after a
third run `$a` is still `true` which is included in `int|string|true` and the
process ends.

## break/continue

The program state from a run of a loop may have `runstate` set to `break` or
`continue`. The case of `continue` is replaced with `next` and handled as
`next`. The case of `break` is replaced with `next` and the program state is
added to the possible output states from the `while` loop and not added to the
state of the loop on entry.

The case of `break N` or `continue N` where _N > 1_ are both handled as
`break`, and _N_ is decremented.

# do-while

Same as a `while` loop except the first run happens unconditionally.

# for loop

Decompiles to a `while` loop, eg:

```
for (stmt1; expr1; stmt2) {
    stmt3;
}
```

is handled the same as

```
stmt1;
while (expr1) {
    stmt3;
    stmt2;
}
```

with one important exception: `stmt2` is executed if the program is in `next`
_or_ `continue` state, not just `next`.

# foreach

eg

```
foreach (expr as $k => $v) {
    stmts
}
```

A `foreach` is handled the same as a `while` loop, however there is no
"condition" which the loop body can assume is true (as in the case of a
`while` loop), so that part doesn't apply.

`expr` is evaluated once before the loop starts. There are three posible cases
for `expr`:

- If `expr` evaluates to an `array`, `$k` is set to `int|string` and `$v` is
  set to the type wrapped by the array, eg if expr was `(Foo|null)[]`, `$v`
  becomes `Foo|null`.
- If `expr` evaluates to an object that implements the `Iterator` or
  `IteratorAggregate` interface, then `$k` is set to the return type of
  `Iterator::key()` and `$v` is set to the return type of
  `Iterator::current()`.
- Otherwise an error is logged and both `$v` and `$k` are set to `mixed`.

In the case of by-reference loop:

```
foreach (expr as $k => &$v) {
    stmts
}
```

`expr` must be an l-value and evaluate to an `array`, `$v` is set to `ref` and
`$k` is set to `int|string`. `ref` is added to the possible var states inside
the array of `expr`.

# switch

The expression at the head of the `switch` block is evaluated and the program
begins stepping through the `case` statements. At each `case` statement, the
`case` expression is evaluated and the program splits in two, one starting on
this `case` statement and another moving onto the next `case` statement. After
the last `case` statement, the program starts at the `default` case. If there
is no `default` case, an empty one is assumed.

This results in _N+1_ program states, where _N_ is the number of `case`
statements. For all program states, the `break` runstate is replaced with
`next` and the program states are merged together.

Note that each `case` branch is run in a state after having evaluated the
previous `case` expressions, and the `default` branch is run in a state after
having evaluated all the `case` expressions. This is important as the
expressions may have side effects.

# return

Simply evaluates the returned expression, if any, and adds it to the list of
possible return values, and sets the program `runstate` to `return`.

# goto

_GOTOs are ignored._

# try-catch-finally

```
try {
    // ...
} catch (A $a) {
    // ...
} catch (B $b) {
    // ...
} finally {
    // ...
}
```

The body of the `try` block is executed normally. Any resulting program states
with `runstate = throw` are considered for the `catch` blocks. For each
`catch` block, if the exception class is in the list of possibly thrown
exceptions, then it is removed, and the program splits, sets `runstate = next`
and sets the catch variable to be an instance of the exception class.

With all possible program states resulting from the `try` and `catch` blocks,
the `finally` block is executed.

For each program state, the `runstate` is captured and reset to `next`. The
program state is sent through the `finally` block. (It may split into multiple
program states due to `ifs` inside the `finally`, etc) For each resulting
program state, if the `runstate` is still `next`, then it is set to whatever
it was before the `finally`.

Here is an example:

```php
try {
    if (bar()) {
        throw new BarException;
    } else {
        $a = 'b';
    }

    // 2 program states:
    // - runstate: next, $a = string
    // - runstate: throw, exceptions: BarException
}

catch (FooException $e) {
    // ignored, since FooException isn't in the list of thrown exceptions
}

catch (BarException $e) {
    // the program state "runstate: throw, exceptions: BarException" enters
    // here
    return 'foo';
    // program state becomes "runstate: return, return: string"
}

// 2 program states:
// - runstate: next, $a = string
// - runstate: return, return: string

finally {
    // 2 program states:
    // - runstate: next, $a = string
    // - runstate: next
    if ($a === 'b') {
        break;
    }
    // 2 program states:
    // - runstate: break, $a = string
    // - runstate: next
}

// 2 program states:
// - runstate: break, $a = string
// - runstate: return, return: string
```

