<?php

namespace JesseSchalken\PhpTypeChecker\Call;

use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\Defns\Class_;
use JesseSchalken\PhpTypeChecker\Expr\Expr;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Type;
use JesseSchalken\PhpTypeChecker\Context;
use JesseSchalken\PhpTypeChecker\Defns;

abstract class Call extends Expr {
    /** @var CallArg[] */
    private $args = [];

    /**
     * @param HasCodeLoc $loc
     * @param CallArg[]  $args
     */
    public function __construct(HasCodeLoc $loc, array $args) {
        parent::__construct($loc);
        $this->args = $args;
    }

    /** @return CallArg[] */
    public function args():array {
        return $this->args;
    }

    public function subStmts():array {
        $stmts = [];
        foreach ($this->args as $arg) {
            $stmts[] = $arg->expr();
        }
        return $stmts;
    }

    protected function unparseArgs():array {
        $args = [];
        foreach ($this->args as $arg) {
            $args[] = $arg->unparse();
        }
        return $args;
    }
}

class FunctionCall extends Call {
    /** @var Expr */
    private $function;

    /**
     * @param HasCodeLoc $loc
     * @param Expr       $function
     * @param CallArg[]  $args
     */
    public function __construct(HasCodeLoc $loc, Expr $function, array $args) {
        parent::__construct($loc, $args);
        $this->function = $function;
    }

    public function subStmts():array {
        return array_merge(parent::subStmts(), [$this->function]);
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\FuncCall(
            $this->function->unparseExprOrName(),
            $this->unparseArgs()
        );
    }

    public function checkExpr(Context\Context $context):Type\Type {
        return $this->function->checkExpr($context)->call(
            $this, $context, $this->args(), false // TODO
        );
    }
}

class StaticMethodCall extends Call {
    /** @var Expr */
    private $class;
    /** @var Expr */
    private $method;

    /**
     * @param HasCodeLoc $loc
     * @param CallArg[]  $args
     * @param Expr       $class
     * @param Expr       $method
     */
    public function __construct(HasCodeLoc $loc, array $args, Expr $class, Expr $method) {
        parent::__construct($loc, $args);
        $this->class  = $class;
        $this->method = $method;
    }

    public function subStmts():array {
        return array_merge(parent::subStmts(), [$this->class, $this->method]);
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\StaticCall(
            $this->class->unparseExprOrName(),
            $this->method->unparseExprOrString(),
            $this->unparseArgs()
        );
    }

    public function checkExpr(Context\Context $context):Type\Type {
        // TODO: Implement getType() method.
    }
}

class MethodCall extends Call {
    /** @var Expr */
    private $object;
    /** @var Expr */
    private $method;

    /**
     * @param HasCodeLoc $loc
     * @param CallArg[]  $args
     * @param Expr       $object
     * @param Expr       $method
     */
    public function __construct(HasCodeLoc $loc, array $args, Expr $object, Expr $method) {
        parent::__construct($loc, $args);
        $this->object = $object;
        $this->method = $method;
    }

    public function subStmts():array {
        return array_merge(parent::subStmts(), [$this->object, $this->method]);
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\MethodCall(
            $this->object->unparseExpr(),
            $this->method->unparseExprOrString(),
            $this->unparseArgs()
        );
    }

    public function checkExpr(Context\Context $context):Type\Type {
        // TODO: Implement getType() method.
    }
}

class CallArg extends Node {
    /** @var Expr */
    private $expr;
    /** @var bool */
    private $splat = false;

    public function __construct(HasCodeLoc $loc, Expr $expr, bool $splat) {
        parent::__construct($loc);
        $this->expr  = $expr;
        $this->splat = $splat;
    }

    public function expr():Expr {
        return $this->expr;
    }

    public function unparse():\PhpParser\Node\Arg {
        return new \PhpParser\Node\Arg(
            $this->expr->unparseExpr(),
            false,
            $this->splat
        );
    }
}

class New_ extends Call {
    /** @var Expr */
    private $class;

    /**
     * @param HasCodeLoc $loc
     * @param Expr       $class
     * @param CallArg[]  $args
     */
    public function __construct(HasCodeLoc $loc, Expr $class, array $args) {
        parent::__construct($loc, $args);
        $this->class = $class;
    }

    public function subStmts():array {
        $stmts   = parent::subStmts();
        $stmts[] = $this->class;
        return $stmts;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\New_(
            $this->class->unparseExprOrName(),
            $this->unparseArgs()
        );
    }

    public function checkExpr(Context\Context $context):Type\Type {
        // TODO: Implement getType() method.
    }
}

class AnonymousNew extends Call {
    /** @var Class_ */
    private $class;

    public function __construct(HasCodeLoc $loc, Class_ $class, array $args) {
        parent::__construct($loc, $args);
        $this->class = $class;
    }

    public function subStmts():array {
        $stmts   = parent::subStmts();
        $stmts[] = $this->class;
        return $stmts;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\New_(
            $this->class->unparseStmt(),
            $this->unparseArgs()
        );
    }

    public function checkExpr(Context\Context $context):Type\Type {
        // TODO: Implement getType() method.
    }
}

