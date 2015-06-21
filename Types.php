<?php

namespace PhpTypeChecker\Foo;

class Base {
    protected function log($message) {

    }
}

abstract class VarState extends Base {
    /**
     * @return Value[]
     */
    abstract function getVal();

    /**
     * @param Value[] $values
     * @return VarState[]
     */
    abstract function setVal(array $values);

    /**
     * @return Value[]
     */
    abstract function getIsset();
}

class Undefined extends VarState {
    function getVal() {
        $this->log('deref of undefined');
        return new Null;
    }

    function setVal(array $values) {
        return $values;
    }

    function getIsset() {
        return [new False];
    }
}

class Ref extends VarState {
    function getVal() {
        return Value::mixed();
    }

    function setVal(array $values) {
        return [$this];
    }

    function getIsset() {
        return [new True, new False];
    }
}

class Value extends VarState {
    /**
     * @return self[]
     */
    static function mixed() {
        return array(
            new Null,
            new Array_,
            new True,
            new False,
            new Resource,
            new String,
            new Int,
            new Float,
        );
    }

    function getVal() {
        return $this;
    }

    function setVal(array $values) {
        return $values;
    }

    function getIsset() {
        return new True;
    }
}

class Null extends Value {
    function getIsset() {
        return new False;
    }
}

class Array_ extends Value {
    /** @var VarState[] */
    private $states = [];

    function getKeyVal() {
        $values = [];
        foreach ($this->states as $state)
            $values = array_merge($values, $state->getVal());
        return $values;
    }

    function setKeyVal(array $values) {
        $states = $this->states;
        foreach ($states as $state)
            $states = array_merge($states, $state->setVal($values));
        $this->states = $states;
    }
}

class Object extends Value {

}

class True extends Value {

}

class False extends Value {

}

class Resource extends Value {

}

class String extends Value {

}

class Int extends Value {

}

class Float extends Value {

}

class VarConfig {
    /** @var VarState[] */
    private $states = array();

    /**
     * @param VarState[] $states
     */
    function __construct(array $states) {
        $this->states = $states;
    }

    /**
     * @param Value[] $value
     */
    function setVal(array $value) {
        $states = array();
        foreach ($this->states as $state)
            $states[] = $state->setVal($value);
        $this->states = $states;
    }

    /**
     * @return Value[]
     */
    function getVal() {
        $values = array();
        foreach ($this->states as $state)
            $values = array_merge($values, $state->getVal());
        return $values;
    }

    /**
     * @return Value[]
     */
    function getIsset() {
        $values = array();
        foreach ($this->states as $state)
            $values = array_merge($values, $state->getIsset());
        return $values;
    }

    function setUnset() {
        $this->states = [new Undefined];
    }

    function setRef() {
        $this->states = [new Ref];
    }
}

class Vars {
    /** @var VarState[][] */
    private $vars = [];

    /**
     * @param string|null $key
     * @param Value $value
     * @param bool $add
     */
    function setVal($key, Value $value, $add) {
        $this->manipulate(
            $key,
            function (VarState $state) use ($value) {
                return $state->setVal($value);
            },
            $add
        );
    }

    /**
     * @param string|null $k
     * @return Value
     */
    function getVal($k) {
    }

    /**
     * @param string|null $k
     */
    function getIsset($k) {
    }

    /**
     * @param string|null $k
     * @param bool $add
     */
    function setUnset($k, $add) {
    }

    /**
     * @param string|null $k
     */
    function makeRef($k) {
    }

    /**
     * @param string|null $key
     * @return VarState[] Manipulable VarState objects for the given key
     */
    function stateOf($key) {
        $key = $key === null ? '~' : ":$key";

        if (!array_key_exists($key, $this->vars)) {
            $states = [];
            foreach ($this->vars as $key => $s)
                foreach ($s as $s2)
                    $states[] = clone $s2;
            $this->vars[$key] = $states;
        }

        return $this->vars[$key];
    }

    function allStates() {
    }

    /**
     * @param string|null $k
     * @param \Closure $f
     * @param bool $add
     */
    function manipulate($k, \Closure $f, $add) {
        $add = $add || $k === null;
        $old = $this->stateOf($k);
        $new = $add || $k == null ? $old : [];
        foreach ($old as $state)
            foreach ($f($state) as $state2)
                $new[] = $state2;
        if ($k === null)
            $this->def = $new;
        else
            $this->vars[$k] = $new;
    }
}

