<?php

namespace JesseSchalken\PhpTypeChecker\Definitions;

use JesseSchalken\PhpTypeChecker\CodeLoc;
use function JesseSchalken\PhpTypeChecker\normalize_constant;
use function JesseSchalken\PhpTypeChecker\str_ieq;
use JesseSchalken\PhpTypeChecker\Type;

class GlobalDefinitions {
    /** @var Type\Type[] */
    private $globals = [];
    /** @var ClassDefinition[] */
    private $classes = [];
    /** @var Function_ */
    private $functions = [];
    /** @var Type\Type */
    private $constants = [];

    public function addGlobal(string $name, Type\Type $type) {
        $this->globals[$name] = $type;
    }

    public function addClass(string $name, ClassDefinition $class) {
        $this->classes[strtolower($name)] = $class;
    }

    public function addFunction(string $name, Type\Function_ $function) {
        $this->functions[strtolower($name)] = $function;
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

    public function hasConstant(string $name):bool {
        return isset($this->constants[normalize_constant($name)]);
    }

    public function getClass(string $name):ClassDefinition {
        return $this->classes[strtolower($name)];
    }

    /**
     * @param string $class
     * @param string $property
     * @param string $context
     * @param bool   $static
     * @return Type\Type|null
     */
    public function getProperty(string $class, string $property, string $context, bool $static) {
    }

    public function getMethod(string $class, string $method, string $context, bool $static) {
    }

    public function isCompatible(string $sub, string $sup):bool {
        if (str_ieq($sub, $sup)) {
            return true;
        } else if ($this->hasClass($sub)) {
            $class = $this->getClass($sub);
            foreach ($class->parents() as $parent) {
                if ($this->isCompatible($parent, $sup)) {
                    return true;
                }
            }
            return false;
        } else {
            return false;
        }
    }
}

abstract class ClassDefinition {
    /** @var Method[] */
    private $methods = [];
    /** @var Property[] */
    private $properties = [];
    /** @var Type\Type[] */
    private $constants = [];
    /** @var bool */
    private $abstract;
    /** @var bool */
    private $final;
    /** @var string[] */
    private $parents;

    public function isAbstract():bool {
        return true;
    }

    public function isFinal():bool {
        return false;
    }

    public function hasConstant(string $name):bool {
        return isset($this->constants[$name]);
    }

    public function getConstant(string $name):Type\Type {
        return $this->constants[$name];
    }

    public function hasMethod(string $name):bool {
        return isset($this->methods[strtolower($name)]);
    }

    public function getMethod(string $name):Method {
        return $this->methods[strtolower($name)];
    }

    public function hasProperty(string $name):bool {
        return isset($this->properties[$name]);
    }

    public function getProperty(string $name):Property {
        return $this->properties[$name];
    }
}

class ClassMember {
    /** @var string */
    private $visibility;
    /** @var bool */
    private $static;

    public function __construct(string $visibility, bool $static) {
        $this->visibility = $visibility;
        $this->static     = $static;
    }

    public function isAccessibleFrom(
        string $class,
        string $context,
        bool $static,
        GlobalDefinitions $defns
    ) {
        if ($this->static != $static) {
            return false;
        }

        switch ($this->visibility) {
            case 'public':
                return true;
            case 'private':
                return str_ieq($class, $context);
            case 'protected':
                return $defns->isCompatible($context, $class);
            default:
                throw new \Exception('huh?');
        }
    }
}

class Method extends ClassMember {
    /** @var Type\Function_ */
    private $type;
    /** @var bool */
    private $abstract;
    /** @var bool */
    private $final;
}

class Property extends ClassMember {
    /** @var Type\Type */
    private $type;

    public function getType():Type\Type {
        return $this->type;
    }
}
