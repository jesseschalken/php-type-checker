

## the plan

- `nikic/php-parser` is used to parse PHP source into an AST.
- `phpdocumentor/reflection-docblock` is used to extract the `@return`, `@param` and `@var` tags from doc comments.
- `JetBrains/phpstorm-stubs` is used to provide types of PHP bulitins.
- All function and method inputs/outputs and class properties are typed statically in the source, to serve as inputs to type checking. Only local variable types need to be inferred.
  - All function/method parameters must be given a type with the `@param` doc comment.
  - All function/method return types must be given a type with the `@return` doc comment.
  - All class properties must be given a type with the `@var` doc comment.
- Local variable types are inferred by stepping through the function/method body and collecting types assigned to each variable.
- Only types supported by PHPDoc are supported ([link](http://www.phpdoc.org/docs/latest/references/phpdoc/types.html)). Notably, this includes arrays of types, eg `Foo[]`, and "or"/disjunction types, eg `Foo|int|string`, but not real generics like `Foo<int>`.
- For disjunction types like `Foo|int|string`, the operations used must be valid for _all_ types in the disjunction, not any. (i.e. prove that the code works always, not that it might work sometimes.)
- Expressions like `$blah instanceof Foo`, `is_string($blah)` or `$blah === null`, when used in a conditional such as `if`, will constrain the types of variables inside the block.
- Since `null` is a distinct type, and `$foo !== null` would infer that `$foo` cannot be `null`, the type checking is effectively null-safe, with `Foo|null` denoting a nullable `Foo`.
- All types for a function parameter must satisfy any type hint on the parameter. So, for example, `/** @param Foo|string $foo */ function f(Foo $foo)` would result in `type 'string' does not match 'Foo'`.
- All `return ...;` statements in a function must satisfy the constraints of the `@return` tag for that function.
- Three operations would be required on an arbitrary expression:
  - `verify()` type-check the expression. Eg `verify("$foo->bar()")` with `$foo = 8` would emit `"cannot call method 'bar' on int"`.
  - `typeof()` infer a type for the expression. Eg `typeof("strlen('foo')")` is `int`.
  - `execute()` infer any types that would be assigned to local variables from evaluating the expression. Eg `execute("$f = true || 8")` returns the type assignment `$f = bool`.
  - `implies()` infer any types that would be assigned to local variables if the expression were to evaluate to something _truthy_. Eg `implies("$f instanceof Foo && is_string($b)")` returns `$f = Foo, $b = string`.
- `int|string` is they type for an array key.
- `int|float` is the type for most numerical operations.


## The PHP memory model

PHP's memory model is split in three parts.

1. Variables
2. Slots
3. Objects

A variable points to a slot. A slot contains a value. A value may point to an object.

### References

Assignment (`$a = ...`) changes the value _in_ the slot pointed to _by_ the variable.
_Reference_ assignment (`$a =& ...`) changes the _variable_ to point to a _different_ slot.

Note that we cannot simply forget about slots and consider `$b =& $a` as causing `$b` to redirect all reads/writes to `$a`:

```php
$a = 9; // The slot that $a points to contains 9
$b =& $a; // $b points to the same slot as $a
unset($a); // $a is disconnected from the slot
$a = 'foo'; // $a points to a new slot containing 'foo'
print $b; // $b is still 9, even though $a is 'foo'
```

Note also that static analysis for PHP programs necessarily needs to understand references. For example, to see that this function will return the integer `5` and not the string `'string'`:

```php
/**
 * @return string
 */
function f() {
  $a = 'string';
  $b =& $a;
  $b = 5;
  return $a;
}
```

### Objects

In the same way variables point to slots, and different variables can point to the same slot, slots themselves can point to objects, and
different slots can point to the same object.

```php
class Foo {
  public $x = 9;
}

$a = new Foo; // The slot that $a points to contains a pointer to an object of type Foo
print $a->x; // 9
$b = $a; // $b points to another slot that contains the same object as the slot pointed to by $a
$a->x = 10; // The 'x' property of the object pointed to by the slot pointed to by $a is now 10
print $b->x; // Prints 10, since the slot pointed to by $b contains a pointer to the same object
```

An object itself is a map from property names to values, in the same way that the set of local variables is a map from variable names
to values. However, since an object belongs to a class which in turn has parent classes, and each class can
have its own private properties even with the same name, a scheme is used to seperate public, private and protected properties.

```php
"\x00$class\x00$property" // Private property for $class
"\x00*\x00$property"      // Protected property
"$property"               // Public property
```

This of course assumes that property names don't contain null bytes (`\x00`). The distinction between protected and public
properties in this scheme may not be necessary.

### Arrays

Arrays are also maps from strings (keys) to slots. However, unlike objects, a slot containing an array contains the array
itself, not a pointer to it. As such, arrays are always passed by value.

# Types

- `Ref[...]` A reference which can only contain the specified type. In terms
  of the PHP memory model, this means the VSlot points to a VStore that may
  be pointed to by (shared with) other VSlots.
- `undefined` The type of undefined variables, properties, and the result of
  `void` functions. In terms of the PHP memory model, this means that the
  VSlot for this name doesn't actually exist.
- `mixed`
    - `int`
        - `constint[9]` An int whose value is known.
    - `string`
        - `conststring[hello]` A string whose value is known.
    - `float`
        - `constfloat[0.1]` A float whose value is known.
    - `true`
    - `false`
    - `resource`
    - `null`
    - `object`
        - `<Class>`
        - `this` The same as `self` except will be the whatever the concrete
          class the method was called on.
        - `static` The same as `self` except will be the 
    - `Closure[..., ...]:...`
    - `(...)[]`

## Composed types

- `...|...|...` Can be any of the listed types.

  Allowed operations are those that are allowed on _all_ of the listed types.
- `...&...&...` Must be all of the listed types, simultaneously.

  Allowed operations are those that are allowed on _any_ of the listed types.

  Typically only satisfiable if the types are classes/interfaces and an object
  is an `instanceof` all of them.

## Aliases

- `bool` => `true|false`
- `boolean` => `bool`
- `integer` => `int`
- `double` => `float`
- `self` => (current class)
- `parent` => (parent class)
- `array` => `mixed[]`
- `$this` => `this`
- `void` => `undefined`

# Syntax

The three types of l-values in PHP are:

```php
$var   # named variable
${...} # dynamic variable

...->prop   # named property
...->${...} # dynamic property

...['foo'] # named key
...[...]   # dynamic key
```

l-values can be both written to (used on the left of `=`) and read from (used as an expression, eg on the right of `=`).

l-values can also be used as a source and destination for references, that is, can be bound as a reference to another l-value, and another l-value can be bound to it.

Functions can also be used as a source and destination for references.

If a function paramter is taken by reference, an l-value must be passed, not just any expression, and it behaves as though the l-value appeared on the right-hand side of the `=&` operator.

If a function returns a reference, then a call to that function may appear on the right hand side of the `=&` operator.

Array literals can include references also, as in `['foo' => &$bar]`, which has the same effect on `$bar` as though `$bar` hed appeared on the right hand side of `=&`.

For loops using references, eg `for ($a as &$b) {...}` have the same effect on both `$a` and `$b` as `$b =& $a[...]` (where `...` is unknown).

A reference in a Closure's `use` list, eg `function () use (&$b) {...}` has the same effect on `$b` as appearing on the right hand side of `=&`. `$b` is set to a reference inside the body of the function.

# Operations

## References

The `deref` operation is defined by:
```
deref Ref[x] = x
deref undefined = error
deref x = x
```

The `toref` operation is defined by:
```
toref Ref[x] = Ref[x]
toref undefined = Ref[null]
toref x = Ref[x]
```

- `$a = expr` 
    - If `$a` is a local variable.
        - If `$a` is a reference, checks that the expression satisfies the
          reference type.
        - Otherwise variable `$a` is set to be `deref(expr)`
    - If `$a` is an object property, `deref(expr)` must be a contained in the
      type of the property, and the type of the property is unchanged.
    - If `$a` is an array entry, `deref(expr)` is added to the list of types
      inside the array.
- `$a =& $b` Variables `$a` and `$b` are set to be `toref($b)`. For both `$a`
  and `$b`:
    - If it is an object property, the type inside the new `Ref[...]` type
      must be _equal_ to the type of the property, and the type of the
      property is left unchanged.
    - If it is an array entry, the new `Ref[...]` type is _added_ to the list
      of possible types inside the array.
- `isset($a)` case `deref(toref($a))`
    - `null` => `false`
    - `...` => `true`
- `unset($a)` `$a` is set to undefined, the effect being:
    - If `$a` is a local variable, it is removed.
    - If `$a` is an array entry, nothing is done, since `undefined` is always
      a possible value of array values.
    - If `$a` is an object property, `undefined` must be a possible type for
      the property.
- `global $foo` Sets `$foo` to be `Ref[...]` using the derefed type of the
  `$foo` global variable.

