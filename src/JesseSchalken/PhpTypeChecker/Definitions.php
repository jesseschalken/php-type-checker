<?php

namespace JesseSchalken\PhpTypeChecker\Definitions;

use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\Node;
use function JesseSchalken\PhpTypeChecker\normalize_constant;
use function JesseSchalken\PhpTypeChecker\str_ieq;
use JesseSchalken\PhpTypeChecker\Type;

class YesNoMaybe {
    const YES   = 1;
    const NO    = -1;
    const MAYBE = 0;
}

class GlobalDefinitions {
    /** @var Type\Type[] */
    private $globals = [];
    /** @var ClassLike[] */
    private $classes = [];
    /** @var Function_ */
    private $functions = [];
    /** @var Type\Type */
    private $constants = [];

    public function addGlobal(string $name, Type\Type $type) {
        $this->globals[$name] = $type;
    }

    public function addClass(ClassLike $class) {
        $this->classes[strtolower($class->name())] = $class;
    }

    public function addFunction(Function_ $function) {
        $this->functions[strtolower($function->name())] = $function;
    }

    public function addConstant(string $name, Type\Type $type) {
        $this->constants[normalize_constant($name)] = $type;
    }

    public function hasGlobal(string $name):bool {
        return isset($this->globals[$name]);
    }

    public function hasClass(string $name):bool {
        return isset($this->classes[strtolower($name)]);
    }

    public function hasFunction(string $name):bool {
        return isset($this->functions[strtolower($name)]);
    }

    public function hasConstnat(string $name):bool {
        return isset($this->constants[normalize_constant($name)]);
    }

    public function getClass(string $name):ClassLike {
        return $this->classes[strtolower($name)];
    }

    public function hasProperty(string $class, string $property, string $context, bool $static):bool {
        return $this->getPropertyType($class, $property, $context, $static) !== null;
    }

    /**
     * @param string $class
     * @param string $property
     * @param string $context
     * @param bool   $static
     * @return Type\Type|null
     */
    public function getPropertyType(string $class, string $property, string $context, bool $static) {
        if (!$this->hasClass($class)) {
            return false;
        }

        $class = $this->getClass($class);
        if ($class->hasProperty($property)) {
            $prop = $class->getProperty($property);
            switch ($prop->getVisibility()) {
                case 'public':
                    break;
                case 'protected':
                    if ($this->isCompatible($context, $class) != YesNoMaybe::YES) {
                        return null;
                    }
                    break;
                case 'private':
                    if (!str_ieq($class->name(), $context)) {
                        return null;
                    }
                    break;
            }
            if ($prop->isStatic() != $static) {
                return null;
            }
            return $prop->getType();
        } else {
            foreach ($class->parents() as $parent) {
                if ($type = $this->getPropertyType($parent, $property, $context, $static)) {
                    return $type;
                }
            }
            return null;
        }
    }

    public function isCompatible(string $sub, string $sup):int {
        if (str_ieq($sub, $sup)) {
            return YesNoMaybe::YES;
        }

        if (!$this->hasClass($sub)) {
            return YesNoMaybe::NO;
        }

        $class = $this->getClass($sub);

        foreach ($class->parents() as $parent) {
            if ($this->isCompatible($parent, $sup) === YesNoMaybe::YES) {
                return YesNoMaybe::YES;
            }
        }

        if ($class->isFinal()) {
            return YesNoMaybe::NO;
        } else {
            return YesNoMaybe::MAYBE;
        }
    }
}

abstract class ClassLike extends Node {
    /** @var string */
    private $name;

    public function __construct(CodeLoc $loc, string $name) {
        parent::__construct($loc);
        $this->name = $name;
    }

    public final function name():string {
        return $this->name;
    }

    public function isAbstract():bool {
        return true;
    }

    public function isFinal():bool {
        return false;
    }

    public function hasConstant(string $name):bool {
        return false;
    }

    public function getConstant(string $name):Type\Type {
        return new Type\Mixed($this->loc());
    }

    /** @return string[] */
    public abstract function parents():array;

    public abstract function hasMethod(string $name):bool;

    public abstract function getMethod(string $name):Method;

    public function hasProperty(string $name):bool {
        return false;
    }

    public abstract function getProperty(string $name):Property;
}

class Class_ extends ClassLike {
    /** @var Method[] */
    private $methods = [];
    /** @var Property[] */
    private $properties = [];
    /** @var Type\Type[] */
    private $constants = [];
    /** @var string[] */
    private $extends = [];
    /** @var string[] */
    private $implements = [];
    /** @var bool */
    private $abstract;
    /** @var bool */
    private $final;

    public function hasConstant(string $name):bool {
        return isset($this->constants[$name]);
    }

    public function getConstant(string $name):Type\Type {
        return $this->constants[$name];
    }

    public function isAbstract():bool {
        return $this->abstract;
    }

    public function isFinal():bool {
        return $this->final;
    }

    /** @return string[] */
    public function parents():array {
        return array_merge($this->extends, $this->implements);
    }

    public function hasMethod(string $name):bool {
        return isset($this->methods[strtolower($name)]);
    }

    public function getMethod(string $name):Method {
        return $this->methods[strtolower($name)];
    }

    public function getProperty(string $name):Property {
        return $this->properties[$name];
    }
}

class Interface_ extends ClassLike {
    /** @var Method[] */
    private $methods;
    /** @var string[] */
    private $extends;

    /** @return string[] */
    public function parents():array {
        return $this->extends;
    }

    public function hasMethod(string $name):bool {
        return isset($this->methods[strtolower($name)]);
    }

    public function getMethod(string $name):Method {
        return $this->methods[strtolower($name)];
    }

    public function getProperty(string $name):Property {
        throw new \Exception('Interfaces don\'t have properties');
    }
}

class Function_ {
    /** @var string */
    private $name;
    /** @var Type\Function_ */
    private $type;

    public function name():string {
        return $this->name;
    }
}

class Method {
    /** @var string */
    private $name;
    /** @var string */
    private $visibility;
    /** @var Type\Function_ */
    private $type;
    /** @var bool */
    private $static;
    /** @var bool */
    private $abstract;
    /** @var bool */
    private $final;
}

class Property {
    /** @var string */
    private $name;
    /** @var string */
    private $visibility;
    /** @var Type\Type */
    private $type;
    /** @var bool */
    private $static;

    public function name():string {
        return $this->name;
    }

    public function getVisibility():string {
        return $this->visibility;
    }

    public function isStatic():bool {
        return $this->static;
    }

    public function getType():Type\Type {
        return $this->type;
    }
}
