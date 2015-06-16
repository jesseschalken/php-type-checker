

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
