<?php

namespace JesseSchalken\PhpTypeChecker\Type;

use JesseSchalken\PhpTypeChecker;
use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\ErrorReceiver;
use JesseSchalken\PhpTypeChecker\Expr;
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

    public function functionExistsNoRef(string $name):bool;

    public function methodExistsNoRef(string $class, string $method, bool $static):bool;
}

class DummyTypeContext implements TypeContext {
    public function isCompatible(string $parent, string $child):bool {
        return str_ieq($parent, $child);
    }

    public function functionExistsNoRef(string $name):bool {
        return false;
    }

    public function methodExistsNoRef(string $class, string $method, bool $static):bool {
        return false;
    }
}

abstract class Type extends PhpTypeChecker\Node {
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

    public final static function union(HasCodeLoc $loc, array $types):self {
        $union = new Union($loc);
        foreach ($types as $type) {
            $union = $union->addType($type, new DummyTypeContext());
        }
        return $union;
    }

    public final static function scalar(HasCodeLoc $loc):self {
        return self::union(
            $loc, [
                new Int_($loc),
                new String_($loc),
                new Float_($loc),
                new SingleValue($loc, true),
                new SingleValue($loc, false),
            ]
        );
    }

    public final static function number(HasCodeLoc $loc):self {
        return self::union(
            $loc, [
                new Int_($loc),
                new String_($loc),
            ]
        );
    }

    public final static function bool(HasCodeLoc $loc):self {
        return self::union(
            $loc, [
                new SingleValue($loc, true),
                new SingleValue($loc, false),
            ]
        );
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

    public final function containsType(self $type, TypeContext $ctx):bool {
        foreach ($type->split() as $type1) {
            if (!$this->containsSingleType($type1, $ctx)) {
                return false;
            }
        }
        return true;
    }

    public abstract function containsSingleType(SingleType $type, TypeContext $ctx):bool;

    /** @return SingleType[] */
    public abstract function split():array;

    public final function __toString():string {
        return $this->toString(false);
    }

    public final function isEmpty():bool {
        return count($this->split()) == 0;
    }

    public final function addType(self $other, TypeContext $ctx):self {
        $self = $this;
        foreach ($other->split() as $state) {
            $self = $self->addSingleType($state, $ctx);
        }
        return $self;
    }

    public final function subtractType(self $other, TypeContext $ctx):self {
        $self = $this;
        foreach ($other->split() as $type) {
            $self = $self->subtractSingleType($type, $ctx);
        }
        return $self;
    }

    public abstract function addSingleType(SingleType $type, TypeContext $ctx):self;

    public abstract function subtractSingleType(SingleType $type, TypeContext $ctx):self;

    /**
     * @param self[]      $vars
     * @param TypeContext $ctx
     * @return Type
     */
    public abstract function fillTypeVars(array $vars, TypeContext $ctx):self;

    public function isCallableMethodOf(Type $type, TypeContext $ctx):bool {
        return false;
    }

    public function hasCallableMethod(string $method, TypeContext $ctx):bool {
        return false;
    }

    /**
     * @param TypeContext   $ctx
     * @param ErrorReceiver $errors
     * @return array|\string[]
     */
    public final function asString(TypeContext $ctx, ErrorReceiver $errors) {
        if (!$this->canCastToString($ctx)) {
            $errors->add("Cannot cast {$this->toString()} to string", $this);
        }
        return $this->asString_($ctx);
    }

    public function asString_(TypeContext $ctx) {
        return [null];
    }

    public function canCastToString(TypeContext $ctx):bool {
        return false;
    }

    public function canUseAsArrayKey():bool {
        return false;
    }

    /**
     * @param ErrorReceiver $errors
     * @return string[]
     */
    public final function asArrayKey(ErrorReceiver $errors):array {
        if (!$this->canUseAsArrayKey()) {
            $errors->add("Cannot use {$this->toString()} as array key", $this);
        }
        return $this->asArrayKey_();
    }

    public function asArrayKey_():array {
        return [null];
    }
}

final class Union extends Type {
    /** @var SingleType[] */
    private $types = [];

    public function toTypeHint() {
        if (count($this->types) == 1) {
            return $this->types[0]->toTypeHint();
        } else {
            return null;
        }
    }

    public function containsSingleType(SingleType $type, TypeContext $ctx):bool {
        foreach ($this->types as $t) {
            if ($t->containsSingleType($type, $ctx)) {
                return true;
            }
        }
        return false;
    }

    /** @return SingleType[] */
    public function split():array {
        return $this->types;
    }

    public final function subtractSingleType(SingleType $type, TypeContext $ctx):self {
        $clone = clone $this;
        foreach ($clone->types as $k => $t) {
            if ($type->containsSingleType($t, $ctx)) {
                unset($clone->types[$k]);
            }
        }
        return $clone;
    }

    public final function addSingleType(SingleType $type, TypeContext $ctx):Type {
        foreach ($this->types as $t) {
            if ($t->containsSingleType($type, $ctx)) {
                return $this;
            }
        }
        $clone          = clone $this->subtractSingleType($type, $ctx);
        $clone->types[] = $type;
        return $clone;
    }

    public final function toString(bool $atomic = false):string {
        $parts = [];
        foreach ($this->split() as $state) {
            $parts[$state->toString($atomic)] = true;
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

                $parts = array_keys($parts);
                sort($parts, SORT_STRING);
                $join = join('|', $parts);
                return $atomic ? "($join)" : $join;
        }
    }

    public function fillTypeVars(array $vars, TypeContext $ctx):self {
        $union = new self($this->loc());
        foreach ($this->types as $type) {
            $union = $union->addType($type->fillTypeVars($vars, $ctx), $ctx);
        }
        return $union;
    }

    public function isCallableMethodOf(Type $type, TypeContext $ctx):bool {
        foreach ($this->types as $t) {
            if (!$t->isCallableMethodOf($type, $ctx)) {
                return false;
            }
        }
        return true;
    }

    public function hasCallableMethod(string $method, TypeContext $ctx):bool {
        foreach ($this->types as $t) {
            if (!$t->hasCallableMethod($method, $ctx)) {
                return false;
            }
        }
        return true;
    }

    public function asString_(TypeContext $ctx):array {
        $strings = [];
        foreach ($this->types as $t) {
            $strings = array_merge($strings, $t->asString_($ctx));
        }
        return $strings;
    }

    public function canCastToString(TypeContext $ctx):bool {
        foreach ($this->types as $t) {
            if (!$t->canCastToString($ctx)) {
                return false;
            }
        }
        return true;
    }
}

abstract class SingleType extends Type {
    public final function split():array {
        return [$this];
    }

    public final function addSingleType(SingleType $type, TypeContext $ctx):Type {
        return (new Union($this->loc()))
            ->addSingleType($type, $ctx)
            ->addSingleType($this, $ctx);
    }

    public final function subtractSingleType(SingleType $type, TypeContext $ctx):Type {
        if ($type->containsSingleType($this, $ctx)) {
            return Type::none($type->loc());
        } else {
            return $this;
        }
    }

    public abstract function containedByTypeVar(string $var, TypeContext $ctx):bool;

    public abstract function containedByConcreteType(ConcreteType $type, TypeContext $ctx):bool;
}

abstract class ConcreteType extends SingleType {
    public abstract function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool;

    public function containedByTypeVar(string $var, TypeContext $ctx):bool {
        return false;
    }

    public final function containedByConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->containsConcreteType($this, $ctx);
    }

    public final function containsSingleType(SingleType $type, TypeContext $ctx):bool {
        return $type->containedByConcreteType($this, $ctx);
    }

    public final function fillTypeVars(array $vars, TypeContext $ctx):self {
        return $this;
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

    public function containedByTypeVar(string $var, TypeContext $ctx):bool {
        return $this->var === $var;
    }

    public function containsSingleType(SingleType $type, TypeContext $ctx):bool {
        return $type->containedByTypeVar($this->var, $ctx);
    }

    public function containedByConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->containsType($this->type, $ctx);
    }

    public function fillTypeVars(array $vars, TypeContext $ctx):self {
        if (isset($vars[$this->var])) {
            // TODO make sure the type matches $this->type
            return $vars[$this->var];
        } else {
            return $this;
        }
    }

    public function isCallableMethodOf(Type $type, TypeContext $ctx):bool {
        return $this->type->isCallableMethodOf($type, $ctx);
    }

    public function hasCallableMethod(string $method, TypeContext $ctx):bool {
        return $this->type->hasCallableMethod($method, $ctx);
    }

    public function asString_(TypeContext $ctx):array {
        return $this->type->asString_($ctx);
    }

    public function canCastToString(TypeContext $ctx):bool {
        return $this->type->canCastToString($ctx);
    }
}

class Mixed extends ConcreteType {
    public function toString(bool $atomic = false):string {
        return 'mixed';
    }

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return true;
    }
}

class SingleValue extends ConcreteType {
    /** @var int|string|bool|float|null */
    private $value;

    /**
     * @param HasCodeLoc                    $loc
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
                return $ctx->functionExistsNoRef($parts[0]);
            case 2:
                return $ctx->methodExistsNoRef($parts[0], $parts[1], true);
            default:
                return false;
        }
    }

    public function isSingleValue($value):bool {
        return $this->value === $value;
    }

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
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
            return $ctx->methodExistsNoRef($val, $method, true);
        } else {
            return false;
        }
    }

    public function canCastToString(TypeContext $ctx):bool {
        return true;
    }

    public function asString_(TypeContext $ctx, ErrorReceiver $errors):array {
        return ["$this->value"];
    }

    public function canUseAsArrayKey():bool {
        return parent::canUseAsArrayKey();
    }

    public function asArrayKey_():array {
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
                return parent::asArrayKey_();
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
class Callable_ extends ConcreteType {
    public function toTypeHint() {
        return 'callable';
    }

    public function toString(bool $atomic = false):string {
        return 'callable';
    }

    public function isCallable(TypeContext $ctx):bool {
        return true;
    }

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->isCallable($ctx);
    }
}

class Float_ extends ConcreteType {
    public function toTypeHint() {
        return 'float';
    }

    public function toString(bool $atomic = false):string {
        return 'float';
    }

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->isFloat();
    }

    public function canCastToString(TypeContext $ctx):bool {
        return true;
    }

    public function canUseAsArrayKey():bool {
        // TODO Floats get destructively truncated to ints when used as an array key. Maybe we should disallow it?
        return true;
    }
}

class String_ extends ConcreteType {
    public function toTypeHint() {
        return 'string';
    }

    public function toString(bool $atomic = false):string {
        return 'string';
    }

    public function isString():bool {
        return true;
    }

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->isString();
    }

    public function canCastToString(TypeContext $ctx):bool {
        return true;
    }

    public function canUseAsArrayKey():bool {
        return true;
    }
}

class Int_ extends ConcreteType {
    public function toTypeHint() {
        return 'int';
    }

    public function toString(bool $atomic = false):string {
        return 'int';
    }

    public function isInt():bool {
        return true;
    }

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->isInt();
    }

    public function canCastToString(TypeContext $ctx):bool {
        return true;
    }

    public function canUseAsArrayKey():bool {
        return true;
    }
}

class Object extends ConcreteType {
    public function toTypeHint() {
        return null;
    }

    public function toString(bool $atomic = false):string {
        return 'object';
    }

    public function isObject():bool {
        return true;
    }

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->isObject();
    }
}

class Resource extends ConcreteType {
    public function toString(bool $atomic = false):string {
        return 'resource';
    }

    public function isResource():bool {
        return true;
    }

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->isResource();
    }

    public function canCastToString(TypeContext $ctx):bool {
        return true;
    }

    public function canUseAsArrayKey():bool {
        // TODO Resources get converted to int when used as an array key. Maybe we should disallow it?
        return true;
    }
}

class Class_ extends ConcreteType {
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

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->isClass($this->class, $ctx);
    }

    public function hasCallableMethod(string $method, TypeContext $ctx):bool {
        return $ctx->methodExistsNoRef($this->class, $method, false);
    }

    public function canCastToString(TypeContext $ctx):bool {
        return $ctx->methodExistsNoRef($this->class, '__toString', false);
    }
}

class Array_ extends ConcreteType {
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

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
        return $type->isArrayOf($this->inner, $ctx);
    }
}

/**
 * A "shape" is an array with a fixed set of (key, type) pairs.
 */
class Shape extends ConcreteType {
    /**
     * @var Type[] Mapping from keys to types
     */
    private $keys = [];

    /**
     * @param HasCodeLoc $loc
     * @param Type[]  $keys
     */
    public function __construct(HasCodeLoc $loc, array $keys = []) {
        parent::__construct($loc);
        $this->keys = $keys;
    }

    public function get(string $key):Type {
        if (isset($this->keys[$key])) {
            return $this->keys[$key];
        } else {
            // TODO Log error
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

    public function containsConcreteType(ConcreteType $type, TypeContext $ctx):bool {
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
}

