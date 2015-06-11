

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
