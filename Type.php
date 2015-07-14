<?php

namespace PhpTypeChecker;

abstract class VarState {
    /**
     * @param static[] $s
     * @return static[]
     */
    static final function unique(array $s) {
        $r = [];
        foreach ($s as $ss)
            if (!$ss->inSet($r))
                $r[] = $ss;
        return $r;
    }

    /**
     * @param static[] $a
     * @param static[] $b
     * @return bool
     */
    static final function eqSet(array $a, array $b) {
        foreach ($a as $aa)
            if (!$aa->inSet($b))
                return false;
        foreach ($b as $bb)
            if (!$bb->inSet($a))
                return false;
        return true;
    }

    /**
     * @param static[] $a
     * @return bool
     */
    final function inSet(array $a) {
        foreach ($a as $as)
            if ($as->eq($this))
                return true;
        return false;
    }

    /**
     * @param VarState $that
     * @return bool
     */
    function eq(VarState $that) {
        return $that instanceof static;
    }

    abstract function unparse();

    /**
     * @param Type[] $types
     * @return VarState[]
     */
    function set(array $types) {
        return $types;
    }

    /** @return Type[] */
    abstract function get();
}

class Ref extends VarState {
    function unparse() { return 'ref'; }

    function set(array $types) {
        return [$this];
    }

    function get() { return [new Mixed]; }
}

class Undefined extends VarState {
    function unparse() { return 'void'; }

    function get() { return [new Null]; }
}

abstract class Type extends VarState {
    function get() { return [$this]; }
}

class Int extends Type {
    function unparse() { return 'int'; }
}

class String extends Type {
    function unparse() { return 'string'; }
}

class Float extends Type {
    function unparse() { return 'float'; }
}

class True extends Type {
    function unparse() { return 'true'; }
}

class False extends Type {
    function unparse() { return 'false'; }
}

class Null extends Type {
    function unparse() { return 'null'; }
}

class Object extends Type {
    function unparse() { return 'object'; }
}

class Mixed extends Type {
    function unparse() { return 'mixed'; }
}

class Class_ extends Object {
    /** @var string */
    private $class;

    /**
     * @param string $class
     */
    function __construct($class) {
        $this->class = $class;
    }

    function eq(VarState $that) {
        return $that instanceof self && $that->class === $this->class;
    }

    function unparse() { return $this->class; }
}

class Callable_ extends Object {
    /** @var VarState */
    private $return;
    /** @var VarState[] */
    private $params = [];

    function __construct() {
        $this->return = new Undefined;
    }

    function arity() {
        return count($this->params);
    }

    function eq(VarState $that) {
        if (!$that instanceof self)
            return false;

        if (!$that->return->eq($this->return))
            return false;

        $arity = max(
            $this->arity(),
            $that->arity()
        );

        for ($i = 0; $i < $arity; $i++)
            if (!$this->param($i)->eq($that->param($i)))
                return false;
        return true;
    }

    /**
     * @param int $i
     * @return VarState
     */
    function param($i) {
        if (array_key_exists($i, $this->params))
            return $this->params[$i];
        else
            return new Undefined;
    }

    function __clone() {
        $this->return = clone $this->return;
        $this->params = clone_array($this->params);
    }

    function unparse() {
        $p = [];
        for ($i = 0; $i < $this->arity(); $i++)
            $p[] = $this->param($i)->unparse();
        $p = join(', ', $p);
        $r = $this->return->unparse();
        return "function ($p): $r";
    }
}

class Resource extends Type {
    function unparse() { return 'resource'; }
}

class Array_ extends Type {
    /** @var Variable */
    private $value;

    function __construct() {
        $this->value = new Variable(false);
    }

    function eq(VarState $that) {
        return $that instanceof self && $that->value->eq($this->value);
    }

    function __clone() {
        $this->value = clone $this->value;
    }

    function unparse() {
        return '(' . $this->value->unparse() . ')[]';
    }
}

class Structure {
    /** @var VarState[][] */
    private $keys = [];
    /** @var VarState[] */
    private $default = [];

    /**
     * @param string|null $key
     * @param callable $f
     */
    function mapStates($key, callable $f) {
        if ($key !== null) {
            if (!array_key_exists($key, $this->keys))
                $this->keys[$key] = clone_array($this->default);

            $this->keys[$key] = array_map_merge($f, $this->keys[$key]);
        } else {
            foreach ($this->keys as $k => $v)
                $this->keys[$k] = array_map_merge($f, $v);

            $this->default = array_merge(
                $this->default,
                array_map_merge($f, clone_array($this->default))
            );
        }
    }

    /**
     * @param string|null $key
     * @return VarState[]
     */
    function getStates($key) {
        if ($key === null) {
            return array_merge(
                $this->default,
                array_merge_many($this->keys)
            );
        } else if (array_key_exists($key, $this->keys)) {
            return $this->keys[$key];
        } else {
            return $this->default;
        }
    }
}

class Variable {
    /** @var VarState[] */
    private $states = [];

    /**
     * @param bool $empty
     */
    function __construct($empty) {
        if (!$empty)
            $this->states[] = new Undefined;
    }

    function __clone() {
        $this->states = clone_array($this->states);
    }

    function eq(self $that) {
        foreach ($this->states as $state1) {
            foreach ($that->states as $state2) {
                if ($state1->eq($state2)) {
                    continue 2;
                }
            }
            return false;
        }
        return true;
    }

    function addMany(array $states) {
        $changed = false;
        foreach ($states as $state) {
            $changed = $this->add($state) || $changed;
        }
        return $changed;
    }

    function add(VarState $state) {
        foreach ($this->states as $state2) {
            if ($state->eq($state2))
                return false;
        }
        $this->states[] = $state;
        return true;
    }

    function import(self $that) {
        return $this->addMany($that->states);
    }

    function unparse() {
        $p = [];
        foreach ($this->states as $s)
            $p[] = $s->unparse();
        sort($p, SORT_STRING);
        return join('|', $p);
    }

    /**
     * @param Type[] $types
     */
    function set(array $types) {
        $states = $this->states;
        $this->states = [];
        foreach ($states as $state)
            $this->addMany($state->set($types));
    }

    function get() {
        /** @var Type[] $types */
        $types = [];
        foreach ($this->states as $state)
            $types = array_merge($types, $state->get());
        return $types;
    }
}
