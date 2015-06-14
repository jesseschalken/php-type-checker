<?php

namespace PhpTypeChecker;

use PhpParser;

function clone_array(array $v) {
    foreach ($v as &$x)
        if (is_object($x))
            $x = clone $x;
        else if (is_array($x))
            $x = clone_array($x);
    return $v;
}

class Type {
    /**
     * @param self[] $types
     * @return self
     */
    static function union(array $types) {
    }

    function contains(self $that) {
        return true;
    }

    function add(self $type) {

    }

    function remove(self $type) {

    }

    function toArray() {
        return array();
    }
}

class Int extends Type {

}

class ConstInt extends Int {
    /** @var int */
    private $value;

    /**
     * @param int $value
     */
    function __construct($value) {
        $this->value = $value;
    }

    function toArray() {
        return array(
            'type'  => 'int',
            'value' => $this->value,
        );
    }
}

class Float extends Type {

}

class String extends Type {

}

class ConstString extends String {

}

class Visibility {
    const PUBLIC_    = 0;
    const PRIVATE_   = 1;
    const PROTECTED_ = 3;
}

class ClassLike {
    /** @var Structure */
    private $object;
    /** @var int[] */
    private $propVisibilities = array();
    /** @var Class_|null */
    private $extends;
    /** @var Interface_[] */
    private $implements = array();
    /** @var Trait_[] */
    private $use = array();
    /** @var Function_[] */
    private $methods = array();
    /** @var int[] */
    private $methodVisibilities = array();
}

class ObjectInstance extends Structure {
    /** @var string */
    private $class;
    /** @var bool */
    private $constructed;

    /**
     * @param string $class
     * @param bool   $constructed
     */
    function __construct($class = 'stdClass', $constructed = true) {
        parent::__construct();
        $this->class       = $class;
        $this->constructed = $constructed;
    }
}

class Class_ extends ClassLike {
}

class Interface_ extends ClassLike {
}

class Trait_ extends ClassLike {
}

class Structure {
    /** @var Type[] */
    private $types = array();
    /** @var Type */
    private $default;

    function __construct() {
        $this->default = new Type;
    }

    function contains(self $that) {
        foreach ($this->types as $k => $t) {
            if (!$t->contains($that->typeOf($k))) {
                return false;
            }
        }

        return $this->allTypes()->contains($that->allTypes());
    }

    function allTypes() {
        return Type::union(array_merge(
            $this->types,
            [$this->default]
        ));
    }

    function defaultType() {
        return $this->default;
    }

    /**
     * @param string $key
     * @return Type
     */
    function typeOf($key) {
        $type =& $this->types[$key];
        $type = $type ?: clone $this->default;
        return $type;
    }

    function set($key, Type $type) {
        $this->types[$key] = $type;
    }

    function add(Type $type) {
        $this->default->add($type);
    }

    function remove($key, Type $type) {
        $this->typeOf($key)->remove($type);
    }

    function toArray() {
        $types = array();
        foreach ($this->types as $k => $type)
            $types[$k] = $type->toArray();
        return array(
            'types'   => $types,
            'default' => $this->default->toArray(),
        );
    }
}

class Function_ {
    /** @var Type[] */
    private $args = array();
    /** @var Type */
    private $return;
    /** @var Structure */
    private $defaults;
    /** @var Structure */
    private $staticVars;

    /**
     * @param Type[] $args
     */
    function map($args) {

    }
}

class ProgramState {
    /** @var ClassLike[] */
    private $classes = array();
    /** @var Function_[] */
    private $functions = array();
    /** @var Structure */
    private $globals;
    /** @var Structure */
    private $locals;
    /** @var Type */
    private $return;
    /** @var Type[] */
    private $constants = array();
    /** @var int|null */
    public $stop;

    function __construct() {
        $this->globals     = new Structure;
        $this->locals      = new Structure;
        $this->staticVars  = new Structure;
        $this->staticProps = new Structure;
    }

    function __clone() {
        $this->constants = clone_array($this->constants);
        $this->classes   = clone_array($this->classes);
        $this->functions = clone_array($this->functions);
        $this->globals   = clone $this->globals;
        $this->locals    = clone $this->locals;
        $this->return    = clone $this->return;
    }

    function toArray() {
        return array(
            'locals' => $this->locals->toArray(),
            'stop'   => $this->stop,
        );
    }

    function typeOf(PhpParser\Node $node) {
        if ($node instanceof PhpParser\Node\Scalar\LNumber) {
            return new ConstInt($node->value);
        } else {
            return null;
        }
    }

    function set(PhpParser\Node $lvalue, Type $type) {
    }

    function execute(PhpParser\Node $node) {
        if ($node instanceof PhpParser\Node\Stmt\Break_) {
            $this->stop = ProgramStop::BREAK_;
        } else if ($node instanceof PhpParser\Node\Stmt\Class_) {
            // not possible
        } else if ($node instanceof PhpParser\Node\Stmt\Interface_) {
            // not possible
        } else if ($node instanceof PhpParser\Node\Stmt\Trait_) {

        } else if ($node instanceof PhpParser\Node\Stmt\Const_) {

        } else if ($node instanceof PhpParser\Node\Stmt\Continue_) {
            $this->stop = ProgramStop::CONTINUE_;
        } else if ($node instanceof PhpParser\Node\Stmt\Do_) {

        } else if ($node instanceof PhpParser\Node\Stmt\Echo_) {

        } else if ($node instanceof PhpParser\Node\Stmt\For_) {

        } else if ($node instanceof PhpParser\Node\Stmt\ForEach_) {

        } else if ($node instanceof PhpParser\Node\Stmt\Function_) {

        } else if ($node instanceof PhpParser\Node\Stmt\Global_) {

        } else if ($node instanceof PhpParser\Node\Stmt\If_) {

        } else if ($node instanceof PhpParser\Node\Stmt\Namespace_) {

        } else if ($node instanceof PhpParser\Node\Stmt\Return_) {
            $this->stop = ProgramStop::RETURN_;
        } else if ($node instanceof PhpParser\Node\Stmt\Static_) {

        } else if ($node instanceof PhpParser\Node\Stmt\Switch_) {

        } else if ($node instanceof PhpParser\Node\Stmt\Throw_) {
            $this->stop = ProgramStop::THROW_;
        } else if ($node instanceof PhpParser\Node\Stmt\TryCatch) {

        } else if ($node instanceof PhpParser\Node\Stmt\Unset_) {

        } else if ($node instanceof PhpParser\Node\Stmt\While_) {

        } else if ($node instanceof PhpParser\Node\Expr\Assign) {

        }
    }
}

class ProgramStop {
    const BREAK_    = 0;
    const RETURN_   = 1;
    const THROW_    = 2;
    const EXIT_     = 3;
    const CONTINUE_ = 4;
}
