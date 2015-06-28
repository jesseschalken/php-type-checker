<?php

namespace PhpTypeChecker;

abstract class VarState {
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
}

class Ref extends VarState {
    function unparse() { return 'ref'; }

    function set(array $types) {
        return [$this];
    }
}

class Undefined extends VarState {
    function unparse() { return 'void'; }
}

abstract class Type extends VarState {
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
        $this->value = new Variable;
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

class Variable {
    /** @var VarState[] */
    private $states = [];

    function __construct() {
        $this->states = [new Undefined];
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
        return join('|', $p);
    }

    /**
     * @param Type[] $types
     */
    function set(array $types) {
        $states       = $this->states;
        $this->states = [];
        foreach ($states as $state)
            $this->addMany($state->set($types));
    }
}
