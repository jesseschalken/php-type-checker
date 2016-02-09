<?php

namespace JesseSchalken\PhpTypeChecker\Function_;

use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Type;

class Function_ extends Node {
    /** @var Type\Type */
    private $returnType;
    /** @var bool */
    private $returnRef;
    /** @var Param[] */
    private $params = [];
    /** @var bool */
    private $variadic;

    /**
     * @param CodeLoc   $loc
     * @param Param[]   $params
     * @param Type\Type $returnType
     * @param bool      $returnByRef
     * @param bool      $variadic
     */
    public function __construct(CodeLoc $loc, array $params, Type\Type $returnType, bool $returnByRef, bool $variadic) {
        parent::__construct($loc);
        $this->returnType = $returnType;
        $this->returnRef  = $returnByRef;
        $this->params     = $params;
        $this->variadic   = $variadic;
    }

    public function subStmts():array {
        $stmts = [];
        foreach ($this->params as $param) {
            foreach ($param->subStmts() as $stmt) {
                $stmts[] = $stmt;
            }
        }
        return $stmts;
    }

    public function toString(string $name):string {
        $params = [];
        foreach ($this->params as $i => $param) {
            $params[] = $param->toString();
        }
        return
            ($this->returnRef ? '&' : '') .
            $name .
            '(' . join(', ', $params) . ($this->variadic ? ' ...' : '') . ')' .
            ':' . $this->returnType->toString();
    }

    public function contains(Function_ $that, Type\TypeContext $ctx):bool {
        if (!$this->returnContains($that, $ctx)) {
            return false;
        }

        $len = max(
            count($this->params),
            count($that->params)
        );

        for ($i = 0; $i < $len; $i++) {
            if (!$this->paramContains($i, $that, $ctx)) {
                return false;
            }
        }

        // Handle a variadic parameter
        if ($this->variadic || $that->variadic) {
            // Not sure what to do besides this. Should do the trick.
            if (!$this->paramContains(9999, $that, $ctx)) {
                return false;
            }
        }

        return true;
    }

    private function returnContains(self $that, Type\TypeContext $ctx):bool {
        if ($this->returnRef != $that->returnRef) {
            // The functions must agree whether to return a reference or not
            // [Dubious. Unlike by-ref parameters, by-ref returns don't have an effect on the called environment.]
            return false;
        }
        if (!$this->returnType->containsType($that->returnType, $ctx)) {
            return false;
        }
        return true;
    }

    private function paramContains(int $i, self $that, Type\TypeContext $ctx):bool {
        if ($this->acceptsParam($i) && !$that->acceptsParam($i)) {
            // The function must be prepared to accept at least as many parameters as us
            // [Dubious. Why shouldn't an overridden function be allowed to ignore some parameters?]
            return false;
        }
        if ($this->isParamOptional($i) && !$that->isParamOptional($i)) {
            // An optional parameter cannot be made required
            return false;
        }
        if ($this->isParamRef($i) != $that->isParamRef($i)) {
            // A by-ref parameter cannot be made by-val or vice versa, because by-ref params have an
            // effect on the calling environment (they cause the variable to be assigned null).
            return false;
        }
        if (!$that->paramType($i)->containsType($this->paramType($i), $ctx)) {
            return false;
        }
        return true;
    }

    public function acceptsParam(int $i):bool {
        return $this->variadic || isset($this->params[$i]);
    }

    public function isParamOptional(int $i):bool {
        if ($this->variadic && $i == count($this->params) - 1) {
            // The last parameter is always optional if this function is variadic
            return true;
        } else if (isset($this->params[$i])) {
            return $this->params[$i]->isOptional();
        } else {
            // Any superfluous parameters are optional
            return true;
        }
    }

    public function paramType(int $i):Type\Type {
        if (isset($this->params[$i])) {
            return $this->params[$i]->type();
        } else if ($this->variadic && $this->params) {
            return $this->params[count($this->params) - 1]->type();
        } else {
            // Any superfluous parameters accept nothing
            return Type\Type::none($this->loc());
        }
    }

    public function isParamRef(int $i):bool {
        if (isset($this->params[$i])) {
            return $this->params[$i]->isByRef();
        } else if ($this->variadic && $this->params) {
            return $this->params[count($this->params) - 1]->isByRef();
        } else {
            // Any superfluous parameters are not passed by reference
            return false;
        }
    }

    public function unparseAttributes():array {
        $params = [];
        foreach ($this->params as $k => $param) {
            $params[] = $param->unparse($k == count($this->params) - 1 && $this->variadic);
        }
        return [
            'byRef'      => $this->returnRef,
            'params'     => $params,
            'returnType' => $this->returnType->toTypeHint(),
        ];
    }
}

class Param extends Node {
    /** @var string */
    private $name;
    /** @var Type\Type */
    private $type;
    /** @var bool */
    private $byRef;
    /** @var Expr\Expr|null */
    private $default = null;

    public function __construct(CodeLoc $loc, string $name, Type\Type $type, bool $byRef, Expr\Expr $default = null) {
        parent::__construct($loc);

        $this->name    = $name;
        $this->type    = $type;
        $this->byRef   = $byRef;
        $this->default = $default;
    }

    public function subStmts():array {
        $default = $this->default;
        return $default ? [$default] : [];
    }

    public function isOptional():bool {
        return $this->default !== null;
    }

    public function isByRef():bool {
        return $this->byRef;
    }

    public function type():Type\Type {
        return $this->type;
    }

    public function toString():string {
        return
            $this->type->toString() . ' ' .
            ($this->byRef ? '&' : '') .
            '$' . $this->name .
            ($this->isOptional() ? '?' : '');
    }

    public function unparse(bool $variadic):\PhpParser\Node\Param {
        return new \PhpParser\Node\Param(
            $this->name,
            $this->default ? $this->default->unparseExpr() : null,
            $this->type->toTypeHint(),
            $this->byRef,
            $variadic
        );
    }
}

