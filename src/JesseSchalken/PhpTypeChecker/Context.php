<?php

namespace JesseSchalken\PhpTypeChecker\Context;

use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\ErrorReceiver;
use JesseSchalken\PhpTypeChecker\Function_;
use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\Type;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Call;
use function JesseSchalken\PhpTypeChecker\normalize_constant;
use function JesseSchalken\PhpTypeChecker\str_ieq;

class Context {
    /** @var ErrorReceiver */
    private $errors;
    /** @var Type\Type[] */
    private $globals = [];
    /** @var Function_\Function_ */
    private $functions = [];
    /** @var Expr\Expr */
    private $constants = [];
    /** @var ClassDecl[] */
    private $classes = [];
    /** @var bool[] */
    private $labels = [];
    /** @var Type\Type[] */
    private $locals;
    /** @var string */
    private $class = '';
    /** @var Type\Type */
    private $return;

    public function __construct(ErrorReceiver $errors) {
        $this->errors = $errors;
        $this->return = new Type\Mixed(new CodeLoc('', 1, 1));
    }

    public function setReturn(Type\Type $type) {
        $this->return = $type;
    }

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

    /**
     * @param string $name
     * @return Function_\Function_|null
     */
    public function getFunction(string $name) {
        return $this->functions[strtolower($name)] ?? null;
    }

    public function functionExists(string $name):bool {
        return !!$this->getFunction($name);
    }

    public function methodExists(string $class, string $method, bool $staticOnly):bool {
        if ($class_ = $this->getClass($class)) {
            if ($method = $class_->methods->get($method)) {
                return $staticOnly && $method->static ? false : true;
            }
        }
        return false;
    }

    public function getGlobal(string $value) {
        return $this->globals[$value] ?? null;
    }

    /**
     * @param HasCodeLoc           $loc
     * @param string               $name
     * @param Call\EvaledCallArg[] $args
     * @param bool                 $noErrors
     * @return Type\Type
     */
    public function callFunction(HasCodeLoc $loc, string $name, array $args, bool $noErrors) {
        $function = $this->getFunction($name);
        if ($function) {
            return $function->call($loc, $this, $args, $noErrors);
        } else {
            return $this->addError("Undefined function '$name'", $loc);
        }
    }

    public function withoutLocals():self {
        $clone         = clone $this;
        $clone->labels = [];
        $clone->locals = [];
        $clone->return = new Type\Mixed(new CodeLoc('', 1, 1));
        return $clone;
    }

    public function withClass(string $class):self {
        $clone        = clone $this;
        $clone->class = $class;
        return $clone;
    }

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

    public function addError(string $message, HasCodeLoc $loc):Type\Type {
        $this->errors->add($message, $loc);
        return Type\Type::none($loc);
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

    public abstract function fromClass(ClassDecl $class):ClassMembers;

    /**
     * @param string  $self
     * @param string  $name
     * @param Context $dfns
     * @return null|string
     */
    public final function findDefiningClass(string $self, string $name, Context $dfns) {
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

    public function fromClass(ClassDecl $class):ClassMembers {
        return $class->properties;
    }
}

class ClassMethods extends ClassMembers {
    /** @var MethodDecl[] */
    private $methods = [];

    public function get(string $name) {
        return $this->methods[strtolower($name)] ?? null;
    }

    public function add(string $method, MethodDecl $obj) {
        $this->methods[strtolower($method)] = $obj;
    }

    public function fromClass(ClassDecl $class):ClassMembers {
        return $class->constants;
    }
}

class ClassConstants extends ClassMembers {
    /** @var Type\Type */
    private $constants = [];

    public function get(string $name) {
        return $this->constants[$name] ?? null;
    }

    public function add(string $name, Type\Type $type) {
        $this->constants[$name] = $type;
    }

    public function fromClass(ClassDecl $class):ClassMembers {
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

