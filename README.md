

== The Plan

- `nikic/php-parser` is used to parse PHP source into an AST.
- `phpdocumentor/reflection-docblock` is used to extract the `@return`, `@param` and `@var` tags from doc comments.
- `JetBrains/phpstorm-stubs` is used to provide types of PHP bulitins.
- All function and method inputs/outputs and class properties are typed statically as inputs to type checking. Only local variable types need to be inferred.
  - All function/method parameters must be given a type with the `@param` doc comment.
  - All function/method return types must be given a type with the `@return` doc comment.
  - All class properties must be given a type with the `@var` doc comment.
- Local variable types are inferred by stepping through the function/method body and collecting types assigned to each variable.
- Only types supported by PHPDoc are supported (see http://www.phpdoc.org/docs/latest/references/phpdoc/types.html). Notably, this includes arrays of types, eg `Foo[]`, and union types, eg `Foo|int|string`, but not real generics like `Foo<int>`.
- For union types like `Foo|int|string`, the operations used must be valid for _all_ types in the union, not any.
- Expressions like `$blah instanceof Foo`, `is_string($blah)` or `$blah === null`, when used in a conditional such as `if`, will constrain the types of variables inside the block.
- All types for a function parameter must satisfy any type hint on the parameter. So, for example, `@param Foo|string $foo` for a parameter with typehint `Foo $foo` would result in `type 'string' does not match 'Foo'`.
- All `return ...;` statements in a function must satisfy the constraints of the `@return` tag for that function.
- Three operations would be required on an arbitrary expression:
  - `verify()` type-check the expression. Eg `verify("$foo->bar()")` with `$foo = 8` would emit `"cannot call method 'bar' on int"`.
  - `typeof()` infer a type for the expression. Eg `typeof("strlen('foo')")` is `int`.
  - `execute()` infer any types that would be assigned to local variables from evaluating the expression. Eg `execute("$f = true || 8")` returns the type assignment `$f = bool`.
  - `implies()` infer any types that would be assigned to local variables if the expression were to evaluate to TRUE. Eg `implies($f instanceof

