<?php

namespace JesseSchalken\PhpTypeChecker\Type;

use JesseSchalken\PhpTypeChecker;
use JesseSchalken\PhpTypeChecker\ErrorReceiver;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Function_;
use JesseSchalken\PhpTypeChecker\Call;
use JesseSchalken\PhpTypeChecker\Context;
use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use function JesseSchalken\PhpTypeChecker\str_ieq;

\phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler(
    'global',
    \phpDocumentor\Reflection\DocBlock\Tag\VarTag::class
);
\phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler(
    'xglobal',
    \phpDocumentor\Reflection\DocBlock\Tag\VarTag::class
);

interface TypeContext {
    public function isCompatible(string $parent, string $child):bool;

    public function functionExists(string $name):bool;

    /**
     * @param string $class
     * @param string $method
     * @param bool   $static If true, non-static methods are excluded, because they (generally) shouldn't be called
     *                       statically.
     * @return bool
     */
    public function methodExists(string $class, string $method, bool $static):bool;
}

class DummyTypeContext implements TypeContext {
    public function isCompatible(string $parent, string $child):bool {
        return str_ieq($parent, $child);
    }

    public function functionExists(string $name):bool {
        return false;
    }

    public function methodExists(string $class, string $method, bool $static):bool {
        return false;
    }
}

abstract class Type extends PhpTypeChecker\Node {
    /**
     * @deprecated Do something with the isFalsy() method on a Type instead.
     * @param HasCodeLoc $loc
     * @return Type
     */
    public final static function falsy(HasCodeLoc $loc):self {
        return self::union($loc, [
            new SingleValue($loc, ''),
            new SingleValue($loc, '0'),
            new SingleValue($loc, 0),
            new SingleValue($loc, 0.0),
            new SingleValue($loc, false),
            new SingleValue($loc, null),
            new Shape($loc, []),
        ]);
    }

    /**
     * @param HasCodeLoc   $loc
     * @param SingleType[] $types
     * @return Type
     */
    public final static function union(HasCodeLoc $loc, array $types):self {
        return count($types) == 1 ? $types[0] : new Union($loc, $types);
    }

    public final static function scalar(HasCodeLoc $loc):self {
        return self::union($loc, [
            new Int_($loc),
            new String_($loc),
            new Float_($loc),
            new SingleValue($loc, true),
            new SingleValue($loc, false),
        ]);
    }

    public final static function number(HasCodeLoc $loc):self {
        return self::union($loc, [
            new Int_($loc),
            new String_($loc),
        ]);
    }

    public final static function bool(HasCodeLoc $loc):self {
        return self::union($loc, [
            new SingleValue($loc, true),
            new SingleValue($loc, false),
        ]);
    }

    public final static function null(HasCodeLoc $loc):self {
        return new SingleValue($loc, null);
    }

    public final static function none(HasCodeLoc $loc):self {
        return new Union($loc);
    }

    public final function removeFalsy(TypeContext $ctx):self {
        return $this->subtractType(self::falsy($this), $ctx);
    }

    public final function removeNull(TypeContext $ctx):self {
        return $this->subtractType(new SingleValue($this, null), $ctx);
    }

    /**
     * @return null|string|\PhpParser\Node\Name
     */
    public function toTypeHint() {
        return null;
    }

    public abstract function toString(bool $atomic = false):string;

    public abstract function containsType(Type $type, TypeContext $ctx):bool;

    /**
     * @return SingleType[]
     */
    public abstract function split():array;

    public final function __toString():string {
        return $this->toString(false);
    }

    public final function isEmpty():bool {
        return count($this->split()) == 0;
    }

    public final function isEquivelant(self $type, TypeContext $ctx):bool {
        return
            $type->containsType($this, $ctx) &&
            $this->containsType($type, $ctx);
    }

    /**
     * Removes any redundant types inside unions.
     * @param TypeContext|null $ctx_
     * @return Type
     */
    public final function simplify(TypeContext $ctx_ = null):self {
        $ctx   = $ctx_ ?? new DummyTypeContext;
        $union = new Union($this);
        foreach ($this->split() as $type) {
            $union = $union->addType($type, $ctx);
        }
        return $union;
    }

    public final function addType(self $other, TypeContext $ctx):self {
        if ($this->containsType($other, $ctx)) {
            return $this;
        } else {
            return self::union($this, array_merge(
                $this->subtractType($other, $ctx)->split(),
                $this->split()
            ));
        }
    }

    public final function subtractType(self $other, TypeContext $ctx):self {
        $types = [];
        foreach ($this->split() as $t) {
            if (!$other->containsType($t, $ctx)) {
                $types[] = $t;
            }
        }
        return self::union($this, $types);
    }

    /**
     * Replace type vars with specific types. Used on the result of a method call to replace "$this" and "static" with
     * the class the method was called on, so method chaining works properly.
     * @param self[]      $vars
     * @param TypeContext $ctx
     * @return Type
     */
    public function fillTypeVars(array $vars, TypeContext $ctx):self {
        return $this;
    }

    public function isCallableMethodOf(Type $type, TypeContext $ctx):bool {
        return false;
    }

    public function hasCallableMethod(string $method, TypeContext $ctx):bool {
        return false;
    }

    /**
     * @param TypeContext   $ctx
     * @param ErrorReceiver $errors
     * @return string[] All the possible strings that would result from casting this to string. Strings can be "null"
     *                  when it is unknown what the result of casting to a string will be (which will be the case for
     *                  everything except SingleValue).
     */
    public function asString(TypeContext $ctx, ErrorReceiver $errors) {
        // By defualt, a type is not allowed to be cast to a string
        $errors->add("Cannot cast {$this->toString()} to string", $this);
        return [null];
    }

    /**
     * @param ErrorReceiver $errors
     * @return string[] All the possible strings that would result from using this as an array key. Strings can be
     *                  "null" if it is unknown what key would be used when this type is used as an array key.
     */
    public function asArrayKey(ErrorReceiver $errors) {
        // By default, a type is not allowed to be used as an array key
        $errors->add("Cannot use {$this->toString()} as array key", $this);
        return [null];
    }

    public function isCallable(TypeContext $ctx):bool {
        return false;
    }

    /**
     * @param int|string|bool|float|null $value
     * @return bool
     */
    public function isSingleValue($value):bool {
        return false;
    }

    public function isString():bool {
        return false;
    }

    public function isFloat():bool {
        return false;
    }

    public function isNull():bool {
        return false;
    }

    public function isBool():bool {
        return false;
    }

    public function isInt():bool {
        return false;
    }

    public function isResource():bool {
        return false;
    }

    public function isFalsy():bool {
        return false;
    }

    public function isTruthy():bool {
        return false;
    }

    public function isArrayOf(Type $type, TypeContext $ctx):bool {
        return false;
    }

    /**
     * @param Type[]      $keys
     * @param TypeContext $ctx
     * @return bool
     */
    public function isShape(array $keys, TypeContext $ctx):bool {
        return false;
    }

    public function isObject():bool {
        return false;
    }

    public function isClass(string $class, TypeContext $ctx):bool {
        return false;
    }

    public function isTypeVar(string $var):bool {
        return false;
    }

    public function addArrayKey(Context\Context $context, Type $type):Type {
        $context->addError("Cannot use $this as array");
        return $this;
    }

    public function setArrayKey(Context\Context $context, string $key, Type $type):Type {
        $context->addError("Cannot use $this as array");
        return $this;
    }

    public function useToSetArrayKey(Context\Context $context, Type $array, Type $value):Type {
        $context->addError("Cannot use $this as array key");
        return $array;
    }

    /**
     * @param HasCodeLoc      $loc
     * @param Context\Context $context
     * @param Call\CallArg[]  $args
     * @param bool            $asRef
     * @return Type
     */
    public function call(HasCodeLoc $loc, Context\Context $context, array $args, bool $asRef):self {
        $context->addError("Cannot call $this as a function", $this);
    }
}

class Union extends Type {
    /** @var SingleType[] */
    private $types = [];

    /**
     * @param HasCodeLoc   $loc
     * @param SingleType[] $types
     */
    public function __construct(HasCodeLoc $loc, array $types = []) {
        parent::__construct($loc);
        $this->types = $types;
    }

    public function toTypeHint() {
        $types = $this->simplify()->split();
        if (count($types) == 1) {
            return $types[0]->toTypeHint();
        } else {
            return null;
        }
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $this->any(function (SingleType $t) use ($type, $ctx) {
            return $t->containsType($type, $ctx);
        });
    }

    /** @return SingleType[] */
    public function split():array {
        return $this->types;
    }

    public final function toString(bool $atomic = false):string {
        $parts = [];
        foreach ($this->simplify()->split() as $type) {
            $parts[$type->toString(false)] = true;
        }
        switch (count($parts)) {
            case 0:
                return '()';
            case 1:
                return array_keys($parts)[0];
            default:
                // Convert true|false to bool
                if (
                    isset($parts['true']) &&
                    isset($parts['false'])
                ) {
                    $parts['bool'] = true;
                    unset($parts['true']);
                    unset($parts['false']);
                }

                ksort($parts, SORT_STRING);
                $join = join('|', array_keys($parts));
                return $atomic ? "($join)" : $join;
        }
    }

    public function useToSetArrayKey(Context\Context $context, Type $array, Type $value):Type {
        return $this->map(function (Type $t) use ($context, $array, $value) {
            return $t->useToSetArrayKey($context, $array, $value);
        });
    }

    public function addArrayKey(Context\Context $context, Type $type):Type {
        return $this->map(function (Type $t) use ($context, $type) {
            return $t->addArrayKey($context, $type);
        });
    }

    public function setArrayKey(Context\Context $context, string $key, Type $type):Type {
        return $this->map(function (Type $t) use ($context, $key, $type) {
            return $t->setArrayKey($context, $key, $type);
        });
    }

    public function fillTypeVars(array $vars, TypeContext $ctx):Type {
        return $this->map(function (Type $t) use ($vars, $ctx) {
            return $t->fillTypeVars($vars, $ctx);
        });
    }

    public function isCallableMethodOf(Type $type, TypeContext $ctx):bool {
        return $this->all(function (SingleType $t) use ($type, $ctx) {
            return $t->isCallableMethodOf($type, $ctx);
        });
    }

    public function hasCallableMethod(string $method, TypeContext $ctx):bool {
        return $this->all(function (SingleType $t) use ($method, $ctx) {
            return $t->hasCallableMethod($method, $ctx);
        });
    }

    public function asString(TypeContext $ctx, ErrorReceiver $errors):array {
        $strings = [];
        foreach ($this->types as $t) {
            foreach ($t->asString($ctx, $errors) as $string) {
                $strings[] = $string;
            }
        }
        return $strings;
    }

    public function asArrayKey(ErrorReceiver $errors):array {
        $keys = [];
        foreach ($this->types as $t) {
            foreach ($t->asArrayKey($errors) as $key) {
                $keys[] = $key;
            }
        }
        return $keys;
    }

    public function isCallable(TypeContext $ctx):bool {
        return $this->all(function (SingleType $t) use ($ctx) {
            return $t->isCallable($ctx);
        });
    }

    public function isSingleValue($value):bool {
        return $this->all(function (SingleType $t) use ($value) {
            return $t->isSingleValue($value);
        });
    }

    public function isString():bool {
        return $this->all(function (SingleType $t) {
            return $t->isString();
        });
    }

    public function isFloat():bool {
        return $this->all(function (SingleType $t) {
            return $t->isFloat();
        });
    }

    public function isNull():bool {
        return $this->all(function (SingleType $t) {
            return $t->isNull();
        });
    }

    public function isFalsy():bool {
        return $this->all(function (SingleType $t) {
            return $t->isFalsy();
        });
    }

    public function isTruthy():bool {
        return $this->all(function (SingleType $t) {
            return $t->isTruthy();
        });
    }

    public function isBool():bool {
        return $this->all(function (SingleType $t) {
            return $t->isBool();
        });
    }

    public function isInt():bool {
        return $this->all(function (SingleType $t) {
            return $t->isInt();
        });
    }

    public function isResource():bool {
        return $this->all(function (SingleType $t) {
            return $t->isResource();
        });
    }

    public function isArrayOf(Type $type, TypeContext $ctx):bool {
        return $this->all(function (SingleType $t) use ($type, $ctx) {
            return $t->isArrayOf($type, $ctx);
        });
    }

    public function isShape(array $keys, TypeContext $ctx):bool {
        return $this->all(function (SingleType $t) use ($keys, $ctx) {
            return $t->isShape($keys, $ctx);
        });
    }

    public function isObject():bool {
        return $this->all(function (SingleType $t) {
            return $t->isObject();
        });
    }

    public function isClass(string $class, TypeContext $ctx):bool {
        return $this->all(function (SingleType $t) use ($class, $ctx) {
            return $t->isClass($class, $ctx);
        });
    }

    public function all(callable $f):bool {
        foreach ($this->types as $t) {
            if (!$f($t)) {
                return false;
            }
        }
        return true;
    }

    public function any(callable $f):bool {
        foreach ($this->types as $t) {
            if ($f($t)) {
                return true;
            }
        }
        return false;
    }

    public function map(callable $f):Type {
        $types = [];
        foreach ($this->types as $t) {
            /** @var Type $type2 */
            $type2 = $f($t);
            foreach ($type2->split() as $type) {
                $types[] = $type;
            }
        }
        return $types;
    }

    public function isTypeVar(string $var):bool {
        return $this->all(function (SingleType $t) use ($var) {
            return $t->isTypeVar($var);
        });
    }
}

/**
 * A type that is not a union.
 */
abstract class SingleType extends Type {
    public final function split():array {
        return [$this];
    }
}

class TypeVar extends SingleType {
    const THIS   = '$this';
    const STATIC = 'static';

    /** @var string */
    private $var;
    /**
     * @var Type The type the type var is contained by, i.e. "Foo" in "class Blah<T extends Foo>". Can be "mixed"
     *      if not specified. This doesn't actually have to be stored here, a map from type vars to constraining
     *      types could be stored somewhere else, but it's more convenient here.
     */
    private $type;

    public function __construct(HasCodeLoc $loc, string $var, Type $type) {
        parent::__construct($loc);
        $this->var  = $var;
        $this->type = $type;
    }

    public function toTypeHint() {
        if ($this->var === self::STATIC) {
            return 'static';
        } else {
            return $this->type->toTypeHint();
        }
    }

    public function toString(bool $atomic = false):string {
        return $this->var;
    }

    public function isFalsy():bool {
        return $this->type->isFalsy();
    }

    public function isTruthy():bool {
        return $this->type->isTruthy();
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isTypeVar($this->var);
    }

    public function isTypeVar(string $var):bool {
        return $this->var === $var;
    }

    public function addArrayKey(Context\Context $context, Type $type):Type {
        return $this->type->addArrayKey($context, $type);
    }

    public function setArrayKey(Context\Context $context, string $key, Type $type):Type {
        return $this->type->setArrayKey($context, $key, $type);
    }

    public function asArrayKey(ErrorReceiver $errors):array {
        return $this->type->asArrayKey($errors);
    }

    public function isCallable(TypeContext $ctx):bool {
        return $this->type->isCallable($ctx);
    }

    public function isSingleValue($value):bool {
        return $this->type->isSingleValue($value);
    }

    public function isString():bool {
        return $this->type->isString();
    }

    public function isFloat():bool {
        return $this->type->isFloat();
    }

    public function isNull():bool {
        return $this->type->isNull();
    }

    public function isBool():bool {
        return $this->type->isBool();
    }

    public function isInt():bool {
        return $this->type->isInt();
    }

    public function isResource():bool {
        return $this->type->isResource();
    }

    public function isArrayOf(Type $type, TypeContext $ctx):bool {
        return $this->type->isArrayOf($type, $ctx);
    }

    public function isShape(array $keys, TypeContext $ctx):bool {
        return $this->type->isShape($keys, $ctx);
    }

    public function isObject():bool {
        return $this->type->isObject();
    }

    public function isClass(string $class, TypeContext $ctx):bool {
        return $this->type->isClass($class, $ctx);
    }

    public function fillTypeVars(array $vars, TypeContext $ctx):Type {
        return $vars[$this->var] ?? $this;
    }

    public function isCallableMethodOf(Type $type, TypeContext $ctx):bool {
        return $this->type->isCallableMethodOf($type, $ctx);
    }

    public function hasCallableMethod(string $method, TypeContext $ctx):bool {
        return $this->type->hasCallableMethod($method, $ctx);
    }

    public function asString(TypeContext $ctx, ErrorReceiver $errors):array {
        return $this->type->asString($ctx, $errors);
    }
}

class Mixed extends SingleType {
    public function toString(bool $atomic = false):string {
        return 'mixed';
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return true;
    }
}

class SingleValue extends SingleType {
    /** @var int|string|bool|float|null */
    private $value;

    /**
     * @param HasCodeLoc                 $loc
     * @param int|string|float|bool|null $value
     */
    public function __construct(HasCodeLoc $loc, $value) {
        parent::__construct($loc);
        $this->value = $value;
    }

    public function toTypeHint() {
        switch (true) {
            case $this->isFloat():
                return 'float';
            case $this->isInt():
                return 'int';
            case $this->isBool():
                return 'bool';
            case $this->isString():
                return 'string';
            case $this->isNull():
                // "void" when PHP gets void return types and the type hint is for a return type
                return null;
            default:
                throw new \Exception('Invalid type for SingleValue: ' . gettype($this->value));
        }
    }

    public function toString(bool $atomic = false):string {
        if ($this->value === null) {
            return 'null';
        } else if (is_bool($this->value)) {
            return $this->value ? 'true' : 'false';
        } else {
            $result = var_export($this->value, true);
            // Make sure a float has a decimal point
            if ($this->isFloat() && strpos($result, '.') === false) {
                $result .= '.0';
            }
            return $result;
        }
    }

    public function isCallable(TypeContext $ctx):bool {
        $parts = explode('::', (string)$this->value, 2);
        switch (count($parts)) {
            case 1:
                return $ctx->functionExists($parts[0]);
            case 2:
                return $ctx->methodExists($parts[0], $parts[1], true);
            default:
                return false;
        }
    }

    public function isSingleValue($value):bool {
        return $this->value === $value;
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isSingleValue($this->value);
    }

    public function isInt():bool {
        return is_int($this->value);
    }

    public function isFloat():bool {
        return is_float($this->value);
    }

    public function isBool():bool {
        return is_bool($this->value);
    }

    public function isString():bool {
        return is_string($this->value);
    }

    public function isNull():bool {
        return is_null($this->value);
    }

    public function isCallableMethodOf(Type $type, TypeContext $ctx):bool {
        $val = $this->value;
        if (is_string($val)) {
            return $type->hasCallableMethod($val, $ctx);
        } else {
            return false;
        }
    }

    public function hasCallableMethod(string $method, TypeContext $ctx):bool {
        $val = $this->value;
        if (is_string($val)) {
            return $ctx->methodExists($val, $method, true);
        } else {
            return false;
        }
    }

    public function asString(TypeContext $ctx, ErrorReceiver $errors):array {
        return ["$this->value"];
    }

    public function asArrayKey(ErrorReceiver $errors):array {
        $value = $this->value;
        switch (true) {
            case is_float($value);
                /** @noinspection PhpMissingBreakStatementInspection */
            case is_bool($value);
                $value = (int)$value;
            case is_string($value):
            case is_int($value):
            case is_null($value):
                return ["$value"];
            default:
                return parent::asArrayKey($errors);
        }
    }

    public function useToSetArrayKey(Context\Context $context, Type $array, Type $value):Type {
        $val = $this->value;
        if (is_bool($val) || is_float($val)) {
            $val = (int)$val;
        }
        return $array->setArrayKey($context, (string)$val, $value);
    }

    public function isFalsy():bool {
        return !$this->value;
    }

    public function isTruthy():bool {
        return !!$this->value;
    }

    public function call(HasCodeLoc $loc, Context\Context $context, array $args, bool $asRef):Type {
        $parts = explode('::', (string)$this->value, 2);
        switch (count($parts)) {
            case 1:
                return $context->callFunction($loc, $parts[0], $args, $asRef);
            case 2:
                // TODO
                // return $globals->callMethod($parts[0], $parts[1], $locals, $errors, $args);
            default:
                return new Mixed($this);
        }
    }
}

/**
 * Either a:
 * - string representing a global function
 * - object implementing the __invoke() method
 * - string of format "class::method"
 * - array of form [$object, 'method']
 * - array of form ['class', 'method']
 */
class Callable_ extends SingleType {
    public function toTypeHint() {
        return 'callable';
    }

    public function toString(bool $atomic = false):string {
        return 'callable';
    }

    public function isCallable(TypeContext $ctx):bool {
        return true;
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isCallable($ctx);
    }

    public function isTruthy():bool {
        // The only possible falsy callable would be a string '0' if there is a global function called '0'.
        // You can't actually define functions starting with digits, so that's impossible.
        return true;
    }

    /**
     * @param HasCodeLoc      $loc
     * @param Context\Context $context
     * @param Call\CallArg[]  $args
     * @param bool            $asRef
     * @return Type
     * @internal param Context\LocalDecls $locals
     * @internal param ErrorReceiver $errors
     */
    public function call(HasCodeLoc $loc, Context\Context $context, array $args, bool $asRef):Type {
        // We don't know what the function signature is going to be, so just eval the args and return mixed
        foreach ($args as $arg) {
            // TODO We should at least check that unpacked parameters are array|Traversable
            $arg->expr()->checkExpr($context);
        }
        return new Mixed($this);
    }
}

class Float_ extends SingleType {
    public function toTypeHint() {
        return 'float';
    }

    public function toString(bool $atomic = false):string {
        return 'float';
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isFloat();
    }

    public function asString(TypeContext $ctx, ErrorReceiver $errors) {
        return [null];
    }

    public function asArrayKey(ErrorReceiver $errors) {
        return [null];
    }
}

class String_ extends SingleType {
    public function toTypeHint() {
        return 'string';
    }

    public function toString(bool $atomic = false):string {
        return 'string';
    }

    public function isString():bool {
        return true;
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isString();
    }

    public function asString(TypeContext $ctx, ErrorReceiver $errors) {
        return [null];
    }

    public function asArrayKey(ErrorReceiver $errors) {
        return [null];
    }
}

class Int_ extends SingleType {
    public function toTypeHint() {
        return 'int';
    }

    public function toString(bool $atomic = false):string {
        return 'int';
    }

    public function isInt():bool {
        return true;
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isInt();
    }

    public function asString(TypeContext $ctx, ErrorReceiver $errors) {
        return [null];
    }

    public function asArrayKey(ErrorReceiver $errors) {
        return [null];
    }
}

class Object extends SingleType {
    public function toTypeHint() {
        return null;
    }

    public function toString(bool $atomic = false):string {
        return 'object';
    }

    public function isObject():bool {
        return true;
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isObject();
    }
}

class Resource extends SingleType {
    public function toString(bool $atomic = false):string {
        return 'resource';
    }

    public function isResource():bool {
        return true;
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isResource();
    }

    public function asString(TypeContext $ctx, ErrorReceiver $errors) {
        return [null];
    }

    public function asArrayKey(ErrorReceiver $errors) {
        return [null];
    }
}

/**
 * An instance of a class or interface.
 */
class Class_ extends SingleType {
    /** @var string */
    private $class;

    public function __construct(HasCodeLoc $loc, string $class) {
        parent::__construct($loc);
        $this->class = $class;
    }

    public function toTypeHint() {
        return new \PhpParser\Node\Name\FullyQualified($this->class);
    }

    public function toString(bool $atomic = false):string {
        return $this->class;
    }

    public function isClass(string $class, TypeContext $ctx):bool {
        return $ctx->isCompatible($class, $this->class);
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isClass($this->class, $ctx);
    }

    public function hasCallableMethod(string $method, TypeContext $ctx):bool {
        return $ctx->methodExists($this->class, $method, false);
    }

    public function asString(TypeContext $ctx, ErrorReceiver $errors) {
        if ($ctx->methodExists($this->class, '__toString', false)) {
            return [null];
        } else {
            return parent::asString($ctx, $errors);
        }
    }
}

class Array_ extends SingleType {
    /** @var Type */
    private $inner;

    public function __construct(HasCodeLoc $loc, Type $inner) {
        parent::__construct($loc);
        $this->inner = $inner;
    }

    public function toTypeHint() {
        return 'array';
    }

    public function toString(bool $atomic = false):string {
        return $this->inner->toString(true) . '[]';
    }

    public function isArrayOf(Type $type, TypeContext $ctx):bool {
        return $type->containsType($this->inner, $ctx);
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isArrayOf($this->inner, $ctx);
    }

    public function addArrayKey(Context\Context $context, Type $type):Type {
        return new self($this, $this->inner->addType($type, $context));
    }

    public function setArrayKey(Context\Context $context, string $key, Type $type):Type {
        return $this->addArrayKey($context, $type);
    }
}

/**
 * A "shape" is an array with a fixed set of (key, type) pairs.
 */
class Shape extends SingleType {
    /**
     * @var Type[] Mapping from keys to types
     */
    private $keys = [];

    /**
     * @param HasCodeLoc $loc
     * @param Type[]     $keys
     */
    public function __construct(HasCodeLoc $loc, array $keys) {
        parent::__construct($loc);
        $this->keys = $keys;
    }

    public function get(string $key):Type {
        if (isset($this->keys[$key])) {
            return $this->keys[$key];
        } else {
            // TODO Log error?
            // Can't be anything
            return new Union($this->loc());
        }
    }

    public function isCallable(TypeContext $ctx):bool {
        if (
            isset($this->keys[0]) &&
            isset($this->keys[1])
        ) {
            return $this->keys[1]->isCallableMethodOf($this->keys[0], $ctx);
        } else {
            return false;
        }
    }

    public function all():Type {
        $type = Type::none($this->loc());
        foreach ($this->keys as $t) {
            $type = $type->addType($t, new DummyTypeContext());
        }
        return $type;
    }

    public function toTypeHint() {
        return 'array';
    }

    public function isArrayOf(Type $type, TypeContext $ctx):bool {
        return $type->containsType($this->all(), $ctx);
    }

    public function addArrayKey(Context\Context $context, Type $type):Type {
        return (new Array_($this, $this->all()))->addArrayKey($context, $type);
    }

    public function setArrayKey(Context\Context $context, string $key, Type $type):Type {
        $keys       = $this->keys;
        $keys[$key] = $type;
        return new self($this, $keys);
    }

    /**
     * @param Type[]      $keys
     * @param TypeContext $ctx
     * @return bool
     */
    public function isShape(array $keys, TypeContext $ctx):bool {
        // Any difference in keys means they are not compatible
        if (
            array_diff_key($keys, $this->keys) ||
            array_diff_key($this->keys, $keys)
        ) {
            return false;
        }

        foreach ($keys as $key => $type) {
            if (!$type->containsType($this->keys[$key], $ctx)) {
                return false;
            }
        }

        return true;
    }

    public function containsType(Type $type, TypeContext $ctx):bool {
        return $type->isShape($this->keys, $ctx);
    }

    public function toString(bool $atomic = false):string {
        $parts = [];
        $assoc = $this->isAssoc();
        foreach ($this->keys as $key => $type) {
            if ($assoc) {
                $parts[] = var_export($key, true) . ' => ' . $type->toString($atomic);
            } else {
                $parts[] = $type->toString($atomic);
            }
        }
        return '[' . join(', ', $parts) . ']';
    }

    public function isAssoc():bool {
        $i = 0;
        foreach ($this->keys as $k => $v) {
            if ($k !== $i++) {
                return true;
            }
        }
        return false;
    }

    public function isFalsy():bool {
        return !$this->keys;
    }

    public function isTruthy():bool {
        return !!$this->keys;
    }
}

