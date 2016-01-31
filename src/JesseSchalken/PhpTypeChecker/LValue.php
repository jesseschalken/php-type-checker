<?php

namespace JesseSchalken\PhpTypeChecker\LValue;

use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\Expr\Expr;

abstract class LValue extends Expr {
    public function isLValue():bool {
        return true;
    }
}

class Variable extends LValue {
    /** @var Expr */
    private $name;

    public function __construct(CodeLoc $loc, Expr $name) {
        parent::__construct($loc);
        $this->name = $name;
    }

    public function subStmts():array {
        return [$this->name];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\Variable($this->name->unparseExprOrString());
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

    public function __construct(CodeLoc $loc, SuperGlobal $global) {
        parent::__construct($loc);
        $this->global = $global;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\Variable($this->global->value());
    }
}

class Property extends LValue {
    /** @var Expr */
    private $object;
    /** @var Expr */
    private $property;

    /**
     * @param CodeLoc $loc
     * @param Expr    $object
     * @param Expr    $property
     */
    public function __construct(CodeLoc $loc, Expr $object, Expr $property) {
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
}

class StaticProperty extends LValue {
    /** @var Expr */
    private $class;
    /** @var Expr */
    private $property;

    public function __construct(CodeLoc $loc, Expr $class, Expr $property) {
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
}

class ArrayAccess extends LValue {
    /** @var Expr */
    private $array;
    /** @var Expr|null */
    private $key;

    /**
     * @param CodeLoc   $loc
     * @param Expr      $array
     * @param Expr|null $key
     */
    public function __construct(CodeLoc $loc, Expr $array, Expr $key = null) {
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
}

class List_ extends Expr {
    /** @var (Expr|null)[] */
    private $exprs;

    /**
     * @param CodeLoc $loc
     * @param (Expr|null)[]   $exprs
     */
    public function __construct(CodeLoc $loc, array $exprs) {
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
        /** @var Expr|null $expr */
        foreach ($this->exprs as $expr) {
            $exprs[] = $expr ? $expr->unparseExpr() : null;
        }
        return new \PhpParser\Node\Expr\List_($exprs);
    }
}

