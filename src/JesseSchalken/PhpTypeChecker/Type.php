<?php

namespace JesseSchalken\PhpTypeChecker\Type;

use JesseSchalken\PhpTypeChecker;
use JesseSchalken\PhpTypeChecker\CodeLoc;
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

    public function methodExistsNoRef(string $class, string $method, bool $stiatc):bool;
}

class DummyTypeContext implements TypeContext {
    public function isCompatible(string $parent, string $child):bool {
        return str_ieq($parent, $child);
    }

    public function functionExistsNoRef(string $name):bool {
        return false;
    }

    public function methodExistsNoRef(string $class, string $method, bool $stiatc):bool {
        return false;
    }
}

abstract class Type extends PhpTypeChecker\Node {
    static function union(CodeLoc $loc, array $types):self {
        $union = new Union($loc);
        foreach ($types as $type) {
            $union = $union->addType($type, new DummyTypeContext());
        }
        return $union;
    }

    static function scalar(CodeLoc $loc):self {
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

    static function number(CodeLoc $loc):self {
        return self::union(
            $loc, [
            new Int_($loc),
            new String_($loc),
        ]
        );
    }

    static function bool(CodeLoc $loc):self {
        return self::union(
            $loc, [
            new SingleValue($loc, true),
            new SingleValue($loc, false),
        ]
        );
    }

    static function null(CodeLoc $loc):self {
        return new SingleValue($loc, null);
    }

    static function none(CodeLoc $loc):self {
        return new Union($loc);
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

    public function __construct(CodeLoc $loc, string $var, Type $type) {
        parent::__construct($loc);
        $this->var  = $var;
        $this->type = $type;
    }

    public function toTypeHint() {
        return $this->type->toTypeHint();
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
     * @param CodeLoc                    $loc
     * @param int|string|float|bool|null $value
     */
    public function __construct(CodeLoc $loc, $value) {
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
            if (is_float($this->value) && strpos($result, '.') === false) {
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
}

class Class_ extends ConcreteType {
    /** @var string */
    private $class;

    public function __construct(CodeLoc $loc, string $class) {
        parent::__construct($loc);
        $this->class  = $class;
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
}

class Array_ extends ConcreteType {
    /** @var Type */
    private $inner;

    public function __construct(CodeLoc $loc, Type $inner) {
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
     * @param CodeLoc $loc
     * @param Type[]  $keys
     */
    public function __construct(CodeLoc $loc, array $keys = []) {
        parent::__construct($loc);
        $this->keys   = $keys;
    }

    public function get(string $key):Type {
        if (isset($this->keys[$key])) {
            return $this->keys[$key];
        } else {
            // TODO Log error
            // Could be anything
            return new Mixed($this->loc());
        }
    }

    public function isCallable(TypeContext $ctx):bool {
        if (
            isset($this->keys[0]) &&
            isset($this->keys[1])
        ) {
            // TODO
        }
        return false;
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

class Function_ extends PhpTypeChecker\Node {
    /** @var Type */
    private $returnType;
    /** @var bool */
    private $returnRef;
    /** @var Param[] */
    private $params = [];
    /** @var bool */
    private $variadic;

    /**
     * @param CodeLoc $loc
     * @param Param[] $params
     * @param Type    $returnType
     * @param bool    $returnRef
     * @param bool    $variadic
     */
    public function __construct(CodeLoc $loc, array $params, Type $returnType, bool $returnRef, bool $variadic) {
        parent::__construct($loc);
        $this->returnType = $returnType;
        $this->returnRef  = $returnRef;
        $this->params     = $params;
        $this->variadic   = $variadic;
    }

    public function toString():string {
        $params = [];
        foreach ($this->params as $i => $param) {
            $params[] = $param->toString();
        }
        return
            ($this->returnRef ? '&' : '') .
            '(' . join(', ', $params) . ($this->variadic ? ' ...' : '') . ')' .
            ':' . $this->returnType->toString();
    }

    public function contains(Function_ $that, TypeContext $ctx):bool {
        if (!$this->returnContains($that, $ctx)) {
            return false;
        }

        $len = max(
            count($this->params),
            count($that->params)
        );

        for ($i = 0; $i < $len; $i++) {
            if (!$this->paramContains($i, $that, $ctx)) {
                return false;
            }
        }

        // Handle a variadic parameter
        if ($this->variadic || $that->variadic) {
            // Not sure what to do besides this. Should do the trick.
            if (!$this->paramContains(9999, $that, $ctx)) {
                return false;
            }
        }

        return true;
    }

    private function returnContains(self $that, TypeContext $ctx):bool {
        if ($this->returnRef != $that->returnRef) {
            // The functions must agree whether to return a reference or not
            // [Dubious. Unlike by-ref parameters, by-ref returns don't have an effect on the called environment.]
            return false;
        }
        if (!$this->returnType->containsType($that->returnType, $ctx)) {
            return false;
        }
        return true;
    }

    private function paramContains(int $i, self $that, TypeContext $ctx):bool {
        if ($this->acceptsParam($i) && !$that->acceptsParam($i)) {
            // The function must be prepared to accept at least as many parameters as us
            // [Dubious. Why shouldn't an overridden function be allowed to ignore some parameters?]
            return false;
        }
        if ($this->isParamOptional($i) && !$that->isParamOptional($i)) {
            // An optional parameter cannot be made required
            return false;
        }
        if ($this->isParamRef($i) != $that->isParamRef($i)) {
            // A by-ref parameter cannot be made by-val or vice versa, because by-ref params have an
            // effect on the calling environment (they cause the variable to be assigned null).
            return false;
        }
        if (!$that->paramType($i)->containsType($this->paramType($i), $ctx)) {
            return false;
        }
        return true;
    }

    public function acceptsParam(int $i):bool {
        return $this->variadic || isset($this->params[$i]);
    }

    public function isParamOptional(int $i):bool {
        if ($this->variadic && $i == count($this->params) - 1) {
            // The last parameter is always optional if this function is variadic
            return true;
        } else if (isset($this->params[$i])) {
            return $this->params[$i]->isOptional();
        } else {
            // Any superfluous parameters are optional
            return true;
        }
    }

    public function paramType(int $i):Type {
        if (isset($this->params[$i])) {
            return $this->params[$i]->type();
        } else if ($this->variadic && $this->params) {
            return $this->params[count($this->params) - 1]->type();
        } else {
            // Any superfluous parameters accept nothing
            return Type::none($this->loc());
        }
    }

    public function isParamRef(int $i):bool {
        if (isset($this->params[$i])) {
            return $this->params[$i]->isRef();
        } else if ($this->variadic && $this->params) {
            return $this->params[count($this->params) - 1]->isRef();
        } else {
            // Any superfluous parameters are not passed by reference
            return false;
        }
    }
}

class Param {
    /** @var Type */
    private $type;
    /** @var bool */
    private $ref;
    /** @var bool */
    private $optional;

    public function __construct(Type $type, bool $ref, bool $optional) {
        $this->type     = $type;
        $this->ref      = $ref;
        $this->optional = $optional;
    }

    public function isOptional():bool {
        return $this->optional;
    }

    public function isRef():bool {
        return $this->ref;
    }

    public function type():Type {
        return $this->type;
    }

    public function toString():string {
        return
            $this->type->toString() . ' ' .
            ($this->ref ? '&' : '') .
            ($this->optional ? '?' : '');
    }
}
