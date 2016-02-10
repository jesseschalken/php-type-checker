<?php

namespace JesseSchalken\PhpTypeChecker\Decls;

use JesseSchalken\PhpTypeChecker\Function_;
use JesseSchalken\PhpTypeChecker\Type;
use JesseSchalken\PhpTypeChecker\Expr;
use function JesseSchalken\PhpTypeChecker\normalize_constant;
use function JesseSchalken\PhpTypeChecker\str_ieq;

class GlobalDecls implements Type\TypeContext {
    /** @var Type\Type[] */
    private $globals = [];
    /** @var Function_\Function_ */
    private $functions = [];
    /** @var Expr\Expr */
    private $constants = [];
    /** @var ClassDecl[] */
    private $classes = [];

    public function addGlobal(string $name, Type\Type $type) {
        $this->globals[$name] = $type;
    }

    public function addFunction(string $name, Function_\Function_ $function) {
        $this->functions[strtolower($name)] = $function;
    }

    public function addConstant(string $name, Expr\Expr $expr) {
        $this->constants[normalize_constant($name)] = $expr;
    }

    public function hasGlobal(string $name):bool {
        return isset($this->globals[$name]);
    }

    public function hasFunction(string $name):bool {
        return isset($this->functions[strtolower($name)]);
    }

    public function hasConstant(string $name):bool {
        return isset($this->constants[normalize_constant($name)]);
    }

    public function isCompatible(string $sub, string $sup):bool {
        if (str_ieq($sub, $sup)) {
            return true;
        }
        foreach ($this->getClassParents($sub) as $parent) {
            if ($this->isCompatible($parent, $sup)) {
                return true;
            }
        }
        return false;
    }

    public function getClass(string $name) {
        return $this->classes[strtolower($name)] ?? null;
    }

    /**
     * @param string $name
     * @return Expr\Expr|null
     */
    public function getConstant(string $name) {
        return $this->constants[$name] ?? null;
    }

    public function getClassParents(string $name):array {
        $class = $this->getClass($name);
        return $class ? $class->parents : [];
    }

    public function functionExistsNoRef(string $name):bool {
        return false;
    }

    public function methodExistsNoRef(string $class, string $method, bool $static):bool {
        return false;
    }

    public function getGlobal(string $value) {
        return $this->globals[$value] ?? null;
    }
}

class LocalDecls {
    /** @var bool[] */
    private $labels = [];
    /** @var Type\Type[] */
    private $locals;

    public function addLocal(string $name, Type\Type $type) {
        $this->locals[$name] = $type;
    }

    public function addLabel(string $name) {
        $this->labels[$name] = true;
    }

    public function hasLabel(string $name):bool {
        return $this->labels[$name] ?? false;
    }

    public function getLocal(string $name) {
        return $this->locals[$name] ?? null;
    }
}

class ClassDecl {
    /** @var ClassConstants */
    public $constants = [];
    /** @var ClassMethods */
    public $methods = [];
    /** @var ClassProperties */
    public $properties = [];
    /** @var bool */
    public $final = false;
    /** @var bool */
    public $abstract = false;
    /** @var string[] */
    public $parents = [];

    public function __construct() {
        $this->constants  = new ClassConstants();
        $this->methods    = new ClassMethods();
        $this->properties = new ClassProperties();
    }
}

abstract class ClassMembers {
    public final function exists(string $name):bool {
        return $this->get($name) !== null;
    }

    /**
     * @param string $name
     * @return ClassMember|object
     * @internal param string $class
     */
    public abstract function get(string $name);

    public abstract function fromClass(ClassDecl $class):self;

    /**
     * @param string      $self
     * @param string      $name
     * @param GlobalDecls $dfns
     * @return null|string
     */
    public final function findDefiningClass(string $self, string $name, GlobalDecls $dfns) {
        if ($this->exists($name)) {
            return $self;
        }
        foreach ($dfns->getClassParents($self) as $parent) {
            $dfn = $dfns->getClass($parent);
            if (!$dfn) {
                continue;
            }
            $result = $this->fromClass($dfn)->findDefiningClass($parent, $name, $dfns);
            if ($result !== null) {
                return $result;
            }
        }
        return null;
    }
}

class ClassProperties extends ClassMembers {
    private $properties = [];

    public function get(string $name) {
        return $this->properties[$name] ?? null;
    }

    public function add(string $name, PropertyDecl $dfn) {
        $this->properties[$name] = $dfn;
    }

    public function fromClass(ClassDecl $class):self {
        return $class->properties;
    }
}

class ClassMethods extends ClassMembers {
    private $methods = [];

    public function get(string $name) {
        return $this->methods[strtolower($name)] ?? null;
    }

    public function add(string $method, MethodDecl $obj) {
        $this->methods[strtolower($method)] = $obj;
    }

    public function fromClass(ClassDecl $class):self {
        return $class->constants;
    }
}

class ClassConstants extends ClassMembers {
    private $constants = [];

    public function get(string $name) {
        return $this->constants[$name] ?? null;
    }

    public function add(string $name, Type\Type $type) {
        $this->constants[$name] = $type;
    }

    public function fromClass(ClassDecl $class):self {
        return $class->constants;
    }
}

class ClassMember {
    /** @var string */
    public $visibility = 'public';
    /** @var bool */
    public $static = false;

    public function __construct(
        string $visibility,
        bool $static
    ) {
        $this->visibility = $visibility;
        $this->static     = $static;
    }
}

class PropertyDecl extends ClassMember {
    /** @var Type\Type */
    public $type;

    public function __construct(
        Type\Type $type,
        string $visibility,
        bool $static
    ) {
        parent::__construct($visibility, $static);

        $this->type = $type;
    }
}

class MethodDecl extends ClassMember {
    /** @var Function_\Function_ */
    public $signature;
    /** @var bool */
    public $final = false;
    /** @var bool */
    public $abstract = false;

    public function __construct(
        Function_\Function_ $signature,
        string $visibility,
        bool $abstract,
        bool $final,
        bool $static
    ) {
        parent::__construct($visibility, $static);

        $this->signature = $signature;
        $this->abstract  = $abstract;
        $this->final     = $final;
    }
}

