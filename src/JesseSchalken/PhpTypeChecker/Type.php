<?php

namespace JesseSchalken\PhpTypeChecker\Type;

use JesseSchalken\PhpTypeChecker;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Function_;
use JesseSchalken\PhpTypeChecker\Call;
use JesseSchalken\PhpTypeChecker\Context;
use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\NullErrorReceiver;
use function JesseSchalken\PhpTypeChecker\str_ieq;
use function JesseSchalken\PhpTypeChecker\merge_types;

\phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler(
    'global',
    \phpDocumentor\Reflection\DocBlock\Tag\VarTag::class
);
\phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler(
    'xglobal',
    \phpDocumentor\Reflection\DocBlock\Tag\VarTag::class
);

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

    /**
     * Typically used for the result of something undefined (method/function/variable/property). Since an error is
     * logged for the undefined thing, the type no longer needs to be tracked and so should be none.
     * @param HasCodeLoc $loc
     * @return Type
     */
    public final static function none(HasCodeLoc $loc):self {
        return new Union($loc);
    }

    public final function removeFalsy(Context\Context $ctx):self {
        return $this->subtractType(self::falsy($this), $ctx);
    }

    public final function removeNull(Context\Context $ctx):self {
        return $this->subtractType(new SingleValue($this, null), $ctx);
    }

    /**
     * @return null|\PhpParser\Node\Name|string
     */
    public function toTypeHint() {
        return null;
    }

    public abstract function toString(bool $atomic = false):string;

    public abstract function containsType(Type $type, Context\Context $ctx):bool;

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

    public final function isEquivelant(self $type, Context\Context $ctx):bool {
        return
            $type->containsType($this, $ctx) &&
            $this->containsType($type, $ctx);
    }

    /**
     * Removes any redundant types inside unions.
     * @param Context\Context|null $ctx_
     * @return Type
     */
    public final function simplify(Context\Context $ctx_ = null):self {
        $ctx   = $ctx_ ?? new Context\Context(new NullErrorReceiver());
        $union = new Union($this);
        foreach ($this->split() as $type) {
            $union = $union->addType($type, $ctx);
        }
        return $union;
    }

    public final function addType(self $other, Context\Context $ctx):self {
        if ($this->containsType($other, $ctx)) {
            return $this;
        } else {
            return self::union($this, array_merge(
                $this->subtractType($other, $ctx)->split(),
                $this->split()
            ));
        }
    }

    public final function subtractType(self $other, Context\Context $ctx):self {
        $types = [];
        foreach ($this->split() as $t) {
            if (!$other->containsType($t, $ctx)) {
                $types[] = $t;
            }
        }
        return self::union($this, $types);
    }

    public final function checkAgainst(HasCodeLoc $loc, self $that, Context\Context $context) {
        $that->checkContains($loc, $this, $context);
    }

    public final function checkContains(HasCodeLoc $loc, self $that, Context\Context $context) {
        if (!$this->containsType($that, $context)) {
            $context->addError("$that is incompatible with $this", $loc);
        }
    }

    public final function checkEquivelant(HasCodeLoc $loc, self $that, Context\Context $context) {
        if (!$this->isEquivelant($that, $context)) {
            $context->addError("$that is not requivelant to $this", $loc);
        }
    }

    /**
     * Replace type vars with specific types. Used on the result of a method call to replace "$this" and "static" with
     * the class the method was called on, so method chaining works properly.
     * @param self[]          $vars
     * @param Context\Context $ctx
     * @return Type
     */
    public function fillTypeVars(array $vars, Context\Context $ctx):self {
        return $this;
    }

    public function isCallableMethodOf(Type $type, Context\Context $ctx):bool {
        return false;
    }

    public function hasCallableMethod(string $method, Context\Context $ctx):bool {
        return false;
    }

    public function getKnownArrayKey(string $key, HasCodeLoc $loc, Context\Context $context, bool $noErrors):Type {
        return $this->getUnknownArrayKey($loc, $context, $noErrors);
    }

    public function getUnknownArrayKey(HasCodeLoc $loc, Context\Context $context, bool $noErrors):Type {
        return $context->addError("Cannot use {$this->toString()} as array", $loc);
    }

    public function useAsArrayKey(HasCodeLoc $loc, Type $array, Context\Context $context, bool $noErrors):Type {
        if (!$noErrors) {
            // By default, a type is not allowed to be used as an array key
            $context->addError("Cannot use {$this->toString()} as array key", $loc);
        }
        return $array->getUnknownArrayKey($loc, $context, $noErrors);
    }

    public function isCallable(Context\Context $ctx):bool {
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

    public function isArrayOf(Type $type, Context\Context $ctx):bool {
        return false;
    }

    /**
     * @param Type[]          $keys
     * @param Context\Context $ctx
     * @return bool
     */
    public function isShape(array $keys, Context\Context $ctx):bool {
        return false;
    }

    public function isObject():bool {
        return false;
    }

    public function isClass(string $class, Context\Context $ctx):bool {
        return false;
    }

    public function isTypeVar(string $var):bool {
        return false;
    }

    public function addArrayKey(HasCodeLoc $loc, Context\Context $context, Type $type, bool $noErrors):Type {
        if (!$noErrors) {
            $context->addError("Cannot use $this as array", $loc);
        }
        return $this;
    }

    public function setArrayKey(HasCodeLoc $loc, Context\Context $context, string $key, Type $type, bool $noErrors):Type {
        if (!$noErrors) {
            $context->addError("Cannot use $this as array", $loc);
        }
        return $this;
    }

    public function useToSetArrayKey(HasCodeLoc $loc, Context\Context $context, Type $array, Type $value, bool $noErrors):Type {
        if (!$noErrors) {
            $context->addError("Cannot use $this as array key", $loc);
        }
        return $array;
    }

    /**
     * @param HasCodeLoc           $loc
     * @param Context\Context      $context
     * @param Call\EvaledCallArg[] $args
     * @param bool                 $noErrors
     * @return Type
     */
    public function call(HasCodeLoc $loc, Context\Context $context, array $args, bool $noErrors):self {
        return $context->addError("Cannot call $this as a function", $this);
    }

    /**
     * Shortcut for `->isEquivelantTo(new Mixed())` that doesn't need a context.
     * @return bool
     */
    public function isExactlyMixed():bool {
        return false;
    }

    public function useAsVariableName(HasCodeLoc $loc, Context\Context $context):Type {
        return $context->addError("$this cannot be used as the name of a variable", $loc);
    }

    public function doForeach(HasCodeLoc $loc, Context\Context $context):ForeachResult {
        $type = $context->addError("$this cannot be used in 'foreach' or ... (unpack).", $loc);
        return new ForeachResult($type, $type);
    }

    /**
     * @param Context\Context $context
     * @return string[]
     */
    public function getStringValues(Context\Context $context):array {
        return [];
    }
}

class ForeachResult {
    /** @var Type */
    public $key;
    /** @var Type */
    public $val;

    public function __construct(Type $key, Type $val) {
        $this->key = $key;
        $this->val = $val;
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

    public function containsType(Type $type, Context\Context $ctx):bool {
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

    public function call(HasCodeLoc $loc, Context\Context $context, array $args, bool $noErrors):Type {
        return $this->map(function (Type $t) use ($loc, $context, $args, $noErrors) {
            return $t->call($loc, $context, $args, $noErrors);
        });
    }

    public function getStringValues(Context\Context $context):array {
        $strings = [];
        foreach ($this->types as $t) {
            foreach ($t->getStringValues($context) as $string) {
                $strings[] = $string;
            }
        }
        return $strings;
    }

    public function useAsVariableName(HasCodeLoc $loc, Context\Context $context):Type {
        return $this->map(function (Type $t) use ($loc, $context) {
            return $t->useAsVariableName($loc, $context);
        });
    }

    public function useToSetArrayKey(HasCodeLoc $loc, Context\Context $context, Type $array, Type $value, bool $noErrors):Type {
        return $this->map(function (Type $t) use ($loc, $context, $array, $value, $noErrors) {
            return $t->useToSetArrayKey($loc, $context, $array, $value, $noErrors);
        });
    }

    public function addArrayKey(HasCodeLoc $loc, Context\Context $context, Type $type, bool $noErrors):Type {
        return $this->map(function (Type $t) use ($loc, $context, $type, $noErrors) {
            return $t->addArrayKey($loc, $context, $type, $noErrors);
        });
    }

    public function setArrayKey(HasCodeLoc $loc, Context\Context $context, string $key, Type $type, bool $noErrors):Type {
        return $this->map(function (Type $t) use ($loc, $context, $key, $type, $noErrors) {
            return $t->setArrayKey($loc, $context, $key, $type, $noErrors);
        });
    }

    public function fillTypeVars(array $vars, Context\Context $ctx):Type {
        return $this->map(function (Type $t) use ($vars, $ctx) {
            return $t->fillTypeVars($vars, $ctx);
        });
    }

    public function isCallableMethodOf(Type $type, Context\Context $ctx):bool {
        return $this->all(function (SingleType $t) use ($type, $ctx) {
            return $t->isCallableMethodOf($type, $ctx);
        });
    }

    public function hasCallableMethod(string $method, Context\Context $ctx):bool {
        return $this->all(function (SingleType $t) use ($method, $ctx) {
            return $t->hasCallableMethod($method, $ctx);
        });
    }

    public function useAsArrayKey(HasCodeLoc $loc, Type $array, Context\Context $context, bool $noErrors):Type {
        return $this->map(function (Type $t) use ($loc, $array, $context, $noErrors) {
            return $t->useAsArrayKey($loc, $array, $context, $noErrors);
        });
    }

    public function isExactlyMixed():bool {
        return $this->all(function (Type $t) {
            return $t->isExactlyMixed();
        });
    }

    public function isCallable(Context\Context $ctx):bool {
        return $this->all(function (SingleType $t) use ($ctx) {
            return $t->isCallable($ctx);
        });
    }

    public function doForeach(HasCodeLoc $loc, Context\Context $context):ForeachResult {
        $empty  = new Union($loc);
        $result = new ForeachResult($empty, $empty);
        foreach ($this->types as $type) {
            $foreach = $type->doForeach($loc, $context);

            $result->key = $result->key->addType($foreach->key, $context);
            $result->val = $result->val->addType($foreach->val, $context);
        }
        return $result;
    }

    public function getKnownArrayKey(string $key, HasCodeLoc $loc, Context\Context $context, bool $noErrors):Type {
        return $this->map(function (Type $t) use ($key, $loc, $context, $noErrors) {
            return $t->getKnownArrayKey($key, $loc, $context, $noErrors);
        });
    }

    public function getUnknownArrayKey(HasCodeLoc $loc, Context\Context $context, bool $noErrors):Type {
        return $this->map(function (Type $t) use ($loc, $context, $noErrors) {
            return $t->getUnknownArrayKey($loc, $context, $noErrors);
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

    public function isArrayOf(Type $type, Context\Context $ctx):bool {
        return $this->all(function (SingleType $t) use ($type, $ctx) {
            return $t->isArrayOf($type, $ctx);
        });
    }

    public function isShape(array $keys, Context\Context $ctx):bool {
        return $this->all(function (SingleType $t) use ($keys, $ctx) {
            return $t->isShape($keys, $ctx);
        });
    }

    public function isObject():bool {
        return $this->all(function (SingleType $t) {
            return $t->isObject();
        });
    }

    public function isClass(string $class, Context\Context $ctx):bool {
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

/**
 * Used for generics. Even though PHP/PhpDoc don't support generics, the '$this' and 'static' types are effectively a
 * kind of type parameter and should be modelled as such.
 */
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

    public function getKnownArrayKey(string $key, HasCodeLoc $loc, Context\Context $context, bool $noErrors):Type {
        return $this->type->getKnownArrayKey($key, $loc, $context, $noErrors);
    }

    public function getUnknownArrayKey(HasCodeLoc $loc, Context\Context $context, bool $noErrors):Type {
        return $this->type->getUnknownArrayKey($loc, $context, $noErrors);
    }

    public function getStringValues(Context\Context $context):array {
        return $this->type->getStringValues($context);
    }

    public function useAsVariableName(HasCodeLoc $loc, Context\Context $context):Type {
        return $this->type->useAsVariableName($loc, $context);
    }

    public function useToSetArrayKey(HasCodeLoc $loc, Context\Context $context, Type $array, Type $value, bool $noErrors):Type {
        return $this->type->useToSetArrayKey($loc, $context, $array, $value, $noErrors);
    }

    public function call(HasCodeLoc $loc, Context\Context $context, array $args, bool $noErrors):Type {
        return $this->type->call($loc, $context, $args, $noErrors);
    }

    public function isExactlyMixed():bool {
        return false;
    }

    public function isFalsy():bool {
        return $this->type->isFalsy();
    }

    public function isTruthy():bool {
        return $this->type->isTruthy();
    }

    public function containsType(Type $type, Context\Context $ctx):bool {
        return $type->isTypeVar($this->var);
    }

    public function isTypeVar(string $var):bool {
        return $this->var === $var;
    }

    public function addArrayKey(HasCodeLoc $loc, Context\Context $context, Type $type, bool $noErrors):Type {
        return $this->type->addArrayKey($loc, $context, $type, $noErrors);
    }

    public function setArrayKey(HasCodeLoc $loc, Context\Context $context, string $key, Type $type, bool $noErrors):Type {
        return $this->type->setArrayKey($loc, $context, $key, $type, $noErrors);
    }

    public function doForeach(HasCodeLoc $loc, Context\Context $context):ForeachResult {
        return $this->type->doForeach($loc, $context);
    }

    public function useAsArrayKey(HasCodeLoc $loc, Type $array, Context\Context $context, bool $noErrors):Type {
        return $this->type->useAsArrayKey($loc, $array, $context, $noErrors);
    }

    public function isCallable(Context\Context $ctx):bool {
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

    public function isArrayOf(Type $type, Context\Context $ctx):bool {
        return $this->type->isArrayOf($type, $ctx);
    }

    public function isShape(array $keys, Context\Context $ctx):bool {
        return $this->type->isShape($keys, $ctx);
    }

    public function isObject():bool {
        return $this->type->isObject();
    }

    public function isClass(string $class, Context\Context $ctx):bool {
        return $this->type->isClass($class, $ctx);
    }

    public function fillTypeVars(array $vars, Context\Context $ctx):Type {
        return $vars[$this->var] ?? $this;
    }

    public function isCallableMethodOf(Type $type, Context\Context $ctx):bool {
        return $this->type->isCallableMethodOf($type, $ctx);
    }

    public function hasCallableMethod(string $method, Context\Context $ctx):bool {
        return $this->type->hasCallableMethod($method, $ctx);
    }
}

class Mixed extends SingleType {
    public function toString(bool $atomic = false):string {
        return 'mixed';
    }

    public function containsType(Type $type, Context\Context $ctx):bool {
        return true;
    }

    public function isExactlyMixed():bool {
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

    public function isCallable(Context\Context $ctx):bool {
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

    public function containsType(Type $type, Context\Context $ctx):bool {
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

    public function isCallableMethodOf(Type $type, Context\Context $ctx):bool {
        return $type->hasCallableMethod((string)$this->value, $ctx);
    }

    public function hasCallableMethod(string $method, Context\Context $ctx):bool {
        return $ctx->methodExists((string)$this->value, $method, true);
    }

    public function useAsArrayKey(HasCodeLoc $loc, Type $array, Context\Context $context, bool $noErrors):Type {
        $value = $this->value;
        switch (true) {
            case is_float($value);
                /** @noinspection PhpMissingBreakStatementInspection */
            case is_bool($value);
                $value = (int)$value;
            case is_string($value):
            case is_int($value):
            case is_null($value):
                return $array->getKnownArrayKey((string)$value, $loc, $context, $noErrors);
            default:
                return parent::useAsArrayKey($loc, $array, $context, $noErrors);
        }
    }

    public function useToSetArrayKey(HasCodeLoc $loc, Context\Context $context, Type $array, Type $value, bool $noErrors):Type {
        $val = $this->value;
        if (is_bool($val) || is_float($val)) {
            $val = (int)$val;
        }
        return $array->setArrayKey($loc, $context, (string)$val, $value, $noErrors);
    }

    public function isFalsy():bool {
        return !$this->value;
    }

    public function isTruthy():bool {
        return !!$this->value;
    }

    public function call(HasCodeLoc $loc, Context\Context $context, array $args, bool $noErrors):Type {
        $parts = explode('::', (string)$this->value, 2);
        switch (count($parts)) {
            case 1:
                return $context->callFunction($loc, $parts[0], $args, $noErrors);
            case 2:
                // TODO
                // return $globals->callMethod($parts[0], $parts[1], $locals, $errors, $args);
            default:
                return $context->addError("Undefined function/method: $this->value", $loc);
        }
    }

    public function useAsVariableName(HasCodeLoc $loc, Context\Context $context):Type {
        $name = (string)$this->value;
        $var  = $context->getLocal($name);
        if ($var && !$var->isEmpty()) {
            return $var;
        } else {
            return $context->addError("Undefined variable: $name", $loc);
        }
    }

    public function getStringValues(Context\Context $context):array {
        return [(string)$this->value];
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

    public function isCallable(Context\Context $ctx):bool {
        return true;
    }

    public function containsType(Type $type, Context\Context $ctx):bool {
        return $type->isCallable($ctx);
    }

    public function isTruthy():bool {
        // The only possible falsy callable would be a string '0' if there is a global function called '0'.
        // You can't actually define functions starting with digits, so that's impossible.
        return true;
    }

    /**
     * @param HasCodeLoc           $loc
     * @param Context\Context      $context
     * @param Call\EvaledCallArg[] $args
     * @param bool                 $noErrors
     * @return Type
     * @internal param bool $asRef
     */
    public function call(HasCodeLoc $loc, Context\Context $context, array $args, bool $noErrors):Type {
        // We don't know what the function signature is going to be, so just return mixed
        return new Mixed($loc);
    }
}

class Float_ extends SingleType {
    public function toTypeHint() {
        return 'float';
    }

    public function toString(bool $atomic = false):string {
        return 'float';
    }

    public function containsType(Type $type, Context\Context $ctx):bool {
        return $type->isFloat();
    }

    public function useAsArrayKey(HasCodeLoc $loc, Type $array, Context\Context $context, bool $noErrors):Type {
        return $array->getUnknownArrayKey($loc, $context, $noErrors);
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

    public function containsType(Type $type, Context\Context $ctx):bool {
        return $type->isString();
    }

    public function useAsArrayKey(HasCodeLoc $loc, Type $array, Context\Context $context, bool $noErrors):Type {
        return $array->getUnknownArrayKey($loc, $context, $noErrors);
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

    public function containsType(Type $type, Context\Context $ctx):bool {
        return $type->isInt();
    }

    public function useAsArrayKey(HasCodeLoc $loc, Type $array, Context\Context $context, bool $noErrors):Type {
        return $array->getUnknownArrayKey($loc, $context, $noErrors);
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

    public function containsType(Type $type, Context\Context $ctx):bool {
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

    public function containsType(Type $type, Context\Context $ctx):bool {
        return $type->isResource();
    }

    public function useAsArrayKey(HasCodeLoc $loc, Type $array, Context\Context $context, bool $noErrors):Type {
        // Apparently you can use resoureces as array keys. Who knew?
        return $array->getUnknownArrayKey($loc, $context, $noErrors);
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

    public function isClass(string $class, Context\Context $ctx):bool {
        return $ctx->isCompatible($class, $this->class);
    }

    public function containsType(Type $type, Context\Context $ctx):bool {
        return $type->isClass($this->class, $ctx);
    }

    public function hasCallableMethod(string $method, Context\Context $ctx):bool {
        return $ctx->methodExists($this->class, $method, false);
    }

    public function getStringValues(Context\Context $context):array {
        // TODO use __toString() if defined on $this->class
        return parent::getStringValues($context);
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

    public function getUnknownArrayKey(HasCodeLoc $loc, Context\Context $context, bool $noErrors):Type {
        return $this->inner;
    }

    public function toString(bool $atomic = false):string {
        if ($this->inner->isExactlyMixed()) {
            return 'array';
        } else {
            return $this->inner->toString(true) . '[]';
        }
    }

    public function isArrayOf(Type $type, Context\Context $ctx):bool {
        return $type->containsType($this->inner, $ctx);
    }

    public function containsType(Type $type, Context\Context $ctx):bool {
        return $type->isArrayOf($this->inner, $ctx);
    }

    public function addArrayKey(HasCodeLoc $loc, Context\Context $context, Type $type, bool $noErrors):Type {
        return new self($this, $this->inner->addType($type, $context));
    }

    public function setArrayKey(HasCodeLoc $loc, Context\Context $context, string $key, Type $type, bool $noErrors):Type {
        return $this->addArrayKey($loc, $context, $type, $noErrors);
    }

    public function doForeach(HasCodeLoc $loc, Context\Context $context):ForeachResult {
        return new ForeachResult(
            new Union($this, [new Int_($this), new String_($this)]),
            $this->inner
        );
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

    public function getKnownArrayKey(string $key, HasCodeLoc $loc, Context\Context $context, bool $noErrors):Type {
        $res = $this->keys[$key] ?? Type::none($loc);
        if (!$noErrors && $res->isEmpty()) {
            $context->addError("Array key '$key' is not defined on $this", $loc);
        }
        return $res;
    }

    public function getUnknownArrayKey(HasCodeLoc $loc, Context\Context $context, bool $noErrors):Type {
        return $this->all($context);
    }

    public function merge(Context\Context $context, self $that):self {
        return new self($this, merge_types($this->keys, $that->keys, $context));
    }

    public function isCallable(Context\Context $ctx):bool {
        if (
            isset($this->keys[0]) &&
            isset($this->keys[1])
        ) {
            return $this->keys[1]->isCallableMethodOf($this->keys[0], $ctx);
        } else {
            return false;
        }
    }

    public function all(Context\Context $context):Type {
        $type = Type::none($this);
        foreach ($this->keys as $t) {
            $type = $type->addType($t, $context);
        }
        return $type;
    }

    public function toTypeHint() {
        return 'array';
    }

    public function doForeach(HasCodeLoc $loc, Context\Context $context):ForeachResult {
        $empty   = new Union($this);
        $foreach = new ForeachResult($empty, $empty);
        foreach ($this->keys as $k => $v) {
            $key = new SingleValue($this, $k);
            $val = $v;

            $foreach->key = $foreach->key->addType($key, $context);
            $foreach->val = $foreach->val->addType($val, $context);
        }
        return $foreach;
    }

    public function isArrayOf(Type $type, Context\Context $ctx):bool {
        return $type->containsType($this->all($ctx), $ctx);
    }

    public function addArrayKey(HasCodeLoc $loc, Context\Context $context, Type $type, bool $noErrors):Type {
        return (new Array_($this, $this->all($context)))->addArrayKey($loc, $context, $type, $noErrors);
    }

    public function setArrayKey(HasCodeLoc $loc, Context\Context $context, string $key, Type $type, bool $noErrors):Type {
        $keys       = $this->keys;
        $keys[$key] = $type;
        return new self($this, $keys);
    }

    /**
     * @param Type[]          $keys
     * @param Context\Context $ctx
     * @return bool
     */
    public function isShape(array $keys, Context\Context $ctx):bool {
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

    public function containsType(Type $type, Context\Context $ctx):bool {
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

