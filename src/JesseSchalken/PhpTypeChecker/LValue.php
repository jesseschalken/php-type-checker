<?php

namespace JesseSchalken\PhpTypeChecker\LValue;

use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Context;
use JesseSchalken\PhpTypeChecker\Defns;
use JesseSchalken\PhpTypeChecker\Constants;
use JesseSchalken\PhpTypeChecker\Type;

abstract class LValue extends Expr\Expr {
    public function isLValue():bool {
        return true;
    }
}

class Variable extends LValue {
    /** @var Expr\Expr */
    private $name;

    public function __construct(HasCodeLoc $loc, Expr\Expr $name) {
        parent::__construct($loc);
        $this->name = $name;
    }

    public function subStmts():array {
        return [$this->name];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\Variable($this->name->unparseExprOrString());
    }

    public function checkExpr(Context\Context $context):Type\Type {
    }
}

class SuperGlobal extends \JesseSchalken\Enum\StringEnum {
    const GLOBALS  = 'GLOBALS';
    const _SERVER  = '_SERVER';
    const _GET     = '_GET';
    const _POST    = '_POST';
    const _FILES   = '_FILES';
    const _COOKIE  = '_COOKIE';
    const _SESSION = '_SESSION';
    const _REQUEST = '_REQUEST';
    const _ENV     = '_ENV';

    public static function values() {
        return [
            self::GLOBALS,
            self::_SERVER,
            self::_GET,
            self::_POST,
            self::_FILES,
            self::_COOKIE,
            self::_SESSION,
            self::_REQUEST,
            self::_ENV,
        ];
    }
}

class SuperGlobalAccess extends LValue {
    /** @var SuperGlobal */
    private $global;

    public function __construct(HasCodeLoc $loc, SuperGlobal $global) {
        parent::__construct($loc);
        $this->global = $global;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\Variable($this->global->value());
    }

    public function checkExpr(Context\Context $context):Type\Type {
        $global = $this->global->value();
        $type   = $context->getGlobal($global);
        if ($type === null) {
            $context->addError("Undefined global '$global'", $this);
            return new Type\Mixed($this);
        } else {
            return $type;
        }
    }
}

class Property extends LValue {
    /** @var Expr\Expr */
    private $object;
    /** @var Expr\Expr */
    private $property;

    /**
     * @param HasCodeLoc $loc
     * @param Expr\Expr  $object
     * @param Expr\Expr  $property
     */
    public function __construct(HasCodeLoc $loc, Expr\Expr $object, Expr\Expr $property) {
        parent::__construct($loc);
        $this->object   = $object;
        $this->property = $property;
    }

    public function isLValue():bool {
        return $this->object->isLValue();
    }

    public function subStmts():array {
        return [$this->object, $this->property];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\PropertyFetch(
            $this->object->unparseExpr(),
            $this->property->unparseExprOrString()
        );
    }

    public function checkExpr(Context\Context $context):Type\Type {
        // TODO: Implement getType() method.
    }
}

class StaticProperty extends LValue {
    /** @var Expr\Expr */
    private $class;
    /** @var Expr\Expr */
    private $property;

    public function __construct(HasCodeLoc $loc, Expr\Expr $class, Expr\Expr $property) {
        parent::__construct($loc);
        $this->class    = $class;
        $this->property = $property;
    }

    public function isLValue():bool {
        return true;
    }

    public function subStmts():array {
        return [$this->class, $this->property];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\StaticPropertyFetch(
            $this->class->unparseExprOrName(),
            $this->property->unparseExprOrString()
        );
    }

    public function checkExpr(Context\Context $context):Type\Type {
        // TODO: Implement getType() method.
    }
}

class ArrayAccess extends LValue {
    /** @var Expr\Expr */
    private $array;
    /** @var Expr\Expr|null */
    private $key;

    /**
     * @param HasCodeLoc     $loc
     * @param Expr\Expr      $array
     * @param Expr\Expr|null $key
     */
    public function __construct(HasCodeLoc $loc, Expr\Expr $array, Expr\Expr $key = null) {
        parent::__construct($loc);
        $this->array = $array;
        $this->key   = $key;
    }

    public function isLValue():bool {
        return $this->array->isLValue();
    }

    public function subStmts():array {
        return $this->key ? [$this->array, $this->key] : [$this->array];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\ArrayDimFetch(
            $this->array->unparseExpr(),
            $this->key ? $this->key->unparseExpr() : null
        );
    }

    public function checkExpr(Context\Context $context):Type\Type {
        // TODO: Implement getType() method.
    }
}

class List_ extends Expr\Expr {
    /** @var (Expr|null)[] */
    private $exprs;

    /**
     * @param HasCodeLoc $loc
     * @param (Expr|null)[]   $exprs
     */
    public function __construct(HasCodeLoc $loc, array $exprs) {
        parent::__construct($loc);
        $this->exprs = $exprs;
    }

    public function subStmts():array {
        $stmts = [];
        foreach ($this->exprs as $expr) {
            if ($expr) {
                $stmts[] = $expr;
            }
        }
        return $stmts;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $exprs = [];
        /** @var Expr\Expr|null $expr */
        foreach ($this->exprs as $expr) {
            $exprs[] = $expr ? $expr->unparseExpr() : null;
        }
        return new \PhpParser\Node\Expr\List_($exprs);
    }

    public function checkExpr(Context\Context $context):Type\Type {
        $items = [];
        foreach ($this->exprs as $k => $v) {
            if ($v) {
                $items[] = new Expr\ArrayItem($v, new Constants\Literal($v, $k), $v);
            }
        }
        return new Expr\Array_($this, $items);
    }
}

