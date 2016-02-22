<?php

namespace JesseSchalken\PhpTypeChecker\Function_;

use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Call;
use JesseSchalken\PhpTypeChecker\Context;
use JesseSchalken\PhpTypeChecker\Type;

class Function_ extends Node {
    /** @var Type\Type */
    private $returnType;
    /** @var bool */
    private $returnRef = false;
    /** @var Param[] */
    private $params = [];
    /** @var Param */
    private $varArg;

    /**
     * @param HasCodeLoc $loc
     * @param Param[]    $params
     * @param Type\Type  $returnType
     * @param bool       $returnByRef
     * @param Param|null $varArg
     */
    public function __construct(
        HasCodeLoc $loc,
        array $params,
        Type\Type $returnType,
        bool $returnByRef,
        Param $varArg = null
    ) {
        parent::__construct($loc);
        $this->returnType = $returnType;
        $this->returnRef  = $returnByRef;
        $this->params     = $params;
        $this->varArg     = $varArg;
    }

    public function addLocals(Context\Context $context) {
        foreach ($this->params as $i => $param) {
            $type = $param->type();
            $name = $param->name();
            if ($this->varArg && $i == count($this->params) - 1) {
                $type = new Type\Array_($param, $type);
            }
            $context->addLocal($name, $type);
        }

        $context->setReturn($this->returnType);
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
        $varArg = $this->varArg;
        foreach ($this->params as $i => $param) {
            $params[] = $param->toString(false);
        }
        if ($varArg) {
            $params[] = $varArg->toString(true);
        }
        return
            ($this->returnRef ? '&' : '') .
            $name .
            '(' . join(', ', $params) . ')' .
            ':' . $this->returnType->toString();
    }

    public function contains(Function_ $that, Context\Context $ctx):bool {
        if (
            $this->returnRef != $that->returnRef ||
            !$this->returnType->containsType($that->returnType, $ctx)
        ) {
            return false;
        }

        $len = max(
            count($this->params),
            count($that->params)
        ) + 1 /* for the variadic param */;

        for ($i = 0; $i < $len; $i++) {
            if (
                $this->isParamOptional($i) &&
                $that->isParamRequired($i)
            ) {
                // Optional/missing parameters cannot be made required
                return false;
            }
            $thatParam = $that->param($i);
            $thisParam = $this->param($i);
            if (!$thisParam) {
                // We don't define this param, so they can do what they want with it, but it can't be required.
                // Continue to make sore any extra params are optional.
                continue;
            }
            if (!$thatParam) {
                // They didn't define this param, but we did. They have to be prepared to accept just as many
                // parameters as us.
                return false;
            }
            if (!$thisParam->contains($thatParam, $ctx)) {
                return false;
            }
        }

        return true;
    }

    public function acceptsParam(int $i):bool {
        return $this->varArg || isset($this->params[$i]);
    }

    /**
     * @param int $i
     * @return Param|null
     */
    public function param(int $i) {
        return $this->params[$i] ?? $this->varArg;
    }

    public function isParamOptional(int $i):bool {
        $param = $this->params[$i] ?? null;
        if ($param) {
            return $param->isOptional();
        } else {
            // Varargs are optional, and superfluous parameters are also optional
            return true;
        }
    }

    public function isParamRequired(int $i):bool {
        return !$this->isParamOptional($i);
    }

    public function paramType(int $i):Type\Type {
        if (isset($this->params[$i])) {
            return $this->params[$i]->type();
        } else if ($this->varArg && $this->params) {
            return $this->params[count($this->params) - 1]->type();
        } else {
            // Any superfluous parameters accept nothing
            return Type\Type::none($this);
        }
    }

    public function isParamRef(int $i):bool {
        if (isset($this->params[$i])) {
            return $this->params[$i]->isByRef();
        } else if ($this->varArg && $this->params) {
            return $this->params[count($this->params) - 1]->isByRef();
        } else {
            // Any superfluous parameters are not passed by reference
            return false;
        }
    }

    public function unparseAttributes():array {
        $params = [];
        $varArg = $this->varArg;
        foreach ($this->params as $k => $param) {
            $params[] = $param->unparse(false);
        }
        if ($varArg) {
            $params[] = $varArg->unparse(true);
        }
        return [
            'byRef'      => $this->returnRef,
            'params'     => $params,
            'returnType' => $this->returnType->toTypeHint(),
        ];
    }

    /**
     * @param HasCodeLoc           $loc
     * @param Context\Context      $context
     * @param Call\EvaledCallArg[] $args
     * @param bool                 $noErrors
     * @return Type\Type
     */
    public function call(HasCodeLoc $loc, Context\Context $context, array $args, bool $noErrors):Type\Type {
        if ($noErrors) {
            return $this->returnType;
        }

        foreach ($this->params as $i => $param) {
            if ($param->isRequired() && !isset($args[$i])) {
                $context->addError("Missing parameter #" . ($i + 1) . " ($param)", $loc);
            }
        }

        $i = 0;
        foreach ($args as $i => $arg) {
            if ($arg->splat) {
                break;
            }
            $param = $this->param($i);
            if ($param) {
                $param->checkAgainst($i, $arg, $context);
            } else {
                $context->addError("Excess parameter #" . ($i + 1), $arg);
            }
        }
        $i++;

        // Splat mode
        // Check all remaining arguments against all remaining parameters
        /** @var Param[] $params */
        $args2  = array_slice($args, $i, null, true);
        $params = array_slice($this->params, $i);
        $varArg = $this->varArg;
        if ($varArg) {
            $params[] = $varArg;
        }
        foreach ($args2 as $i => $arg) {
            foreach ($params as $param) {
                $param->checkAgainst($i, $arg, $context);
            }
        }

        return $this->returnType;
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

    public function __construct(HasCodeLoc $loc, string $name, Type\Type $type, bool $byRef, Expr\Expr $default = null) {
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

    public function isRequired():bool {
        return $this->default === null;
    }

    public function isOptional():bool {
        return $this->default !== null;
    }

    public function contains(self $other, Context\Context $context) {
        return
            $other->byRef == $this->byRef &&
            $other->type->containsType($this->type, $context);
    }

    public function isByRef():bool {
        return $this->byRef;
    }

    public function type():Type\Type {
        return $this->type;
    }

    public function toString(bool $variadic):string {
        return
            $this->type->toString() . ' ' .
            ($this->byRef ? '&' : '') .
            ($variadic ? '...' : '') .
            '$' . $this->name .
            ($this->isOptional() ? ' = ?' : '');
    }

    public function checkAgainst(int $i, Call\EvaledCallArg $arg, Context\Context $context) {
        if ($this->byRef) {
            if (!$arg->referrable) {
                $context->addError("Argument #" . ($i + 1) . " must be referrable (lvalue or call).", $arg);
            }
            $this->type->checkEquivelant($arg, $arg->type, $context);
        } else {
            $this->type->checkContains($arg, $arg->type, $context);
        }
    }

    public function __toString() {
        return $this->toString(false);
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

    public function name():string {
        return $this->name;
    }
}

