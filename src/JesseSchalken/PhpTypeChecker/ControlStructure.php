<?php

namespace JesseSchalken\PhpTypeChecker\ControlStructure;

use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\Decls;
use JesseSchalken\PhpTypeChecker\Defns;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Parser;
use JesseSchalken\PhpTypeChecker\Stmt;
use JesseSchalken\PhpTypeChecker\Type;
use function JesseSchalken\MagicUtils\clone_ref;
use function JesseSchalken\PhpTypeChecker\extract_namespace;
use function JesseSchalken\PhpTypeChecker\recursive_scan2;
use function JesseSchalken\PhpTypeChecker\remove_namespace;

abstract class ControlStructure extends Stmt\SingleStmt {
}

class DoWhile extends ControlStructure {
    /** @var Stmt\Block */
    private $body;
    /** @var Expr\Expr */
    private $cond;

    /**
     * @param CodeLoc    $loc
     * @param Stmt\Block $body
     * @param Expr\Expr  $cond
     */
    public function __construct(CodeLoc $loc, Stmt\Block $body, Expr\Expr $cond) {
        parent::__construct($loc);
        $this->body = $body;
        $this->cond = $cond;
    }

    public function subStmts():array {
        return [$this->body, $this->cond];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\While_(
            $this->cond->unparseExpr(),
            $this->body->unparseNodes()
        );
    }
}

class If_ extends ControlStructure {
    /** @var Expr\Expr */
    private $cond;
    /** @var Stmt\Block */
    private $true;
    /** @var Stmt\Block */
    private $false;

    public function __construct(CodeLoc $loc, Expr\Expr $cond, Stmt\Block $true, Stmt\Block $false) {
        parent::__construct($loc);
        $this->cond  = $cond;
        $this->true  = $true;
        $this->false = $false;
    }

    public function subStmts():array {
        return [$this->cond, $this->true, $this->false];
    }

    public function unparseStmt() {
        $elseIfs = [];
        $else    = $this->false->unparseNodes();

        if (count($else) == 1) {
            $if_ = $else[0];
            if ($if_ instanceof \PhpParser\Node\Stmt\If_) {
                $elseIfs = array_merge(
                    [new \PhpParser\Node\Stmt\ElseIf_(
                        $if_->cond,
                        $if_->stmts
                    )],
                    $if_->elseifs
                );
                $else    = $if_->else ? $if_->else->stmts : [];
            }
        }

        return new \PhpParser\Node\Stmt\If_(
            $this->cond->unparseExpr(),
            [
                'stmts'   => $this->true->unparseNodes(),
                'elseifs' => $elseIfs,
                'else'    => $else ? new \PhpParser\Node\Stmt\Else_($else) : null,
            ]
        );
    }
}

class Foreach_ extends ControlStructure {
    /** @var Expr\Expr */
    private $array;
    /** @var Expr\Expr|null */
    private $key;
    /** @var Expr\Expr */
    private $value;
    /** @var Stmt\Block */
    private $body;
    /** @var bool */
    private $byRef;

    /**
     * @param CodeLoc        $loc
     * @param Expr\Expr      $array
     * @param Expr\Expr|null $key
     * @param Expr\Expr      $value
     * @param bool           $byRef
     * @param Stmt\Block     $body
     */
    public function __construct(
        CodeLoc $loc,
        Expr\Expr $array,
        Expr\Expr $key = null,
        Expr\Expr $value,
        bool $byRef,
        Stmt\Block $body
    ) {
        parent::__construct($loc);
        $this->array = $array;
        $this->key   = $key;
        $this->value = $value;
        $this->body  = $body;
        $this->byRef = $byRef;
    }

    public function subStmts():array {
        $stmts = [
            $this->array,
            $this->value,
            $this->body,
        ];
        if ($this->key) {
            $stmts[] = $this->key;
        }
        return $stmts;
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Foreach_(
            $this->array->unparseExpr(),
            $this->value->unparseExpr(),
            [
                'keyVar' => $this->key ? $this->key->unparseExpr() : null,
                'byRef'  => $this->byRef,
                'stmts'  => $this->body->unparseNodes(),
            ]
        );
    }
}

/**
 * Special statements used for the init/cond/loop parts of a for loop.
 * Is like the comma operator in C and JavaScript but can actually only
 * be used in the head of a for loop.
 */
class ForComma extends Stmt\Stmt {
    /** @var Expr\Expr[] */
    private $exprs = [];

    /**
     * @param CodeLoc     $loc
     * @param Expr\Expr[] $exprs
     */
    public function __construct(CodeLoc $loc, array $exprs) {
        parent::__construct($loc);
        $this->exprs = $exprs;
    }

    public function subStmts():array {
        return $this->exprs;
    }

    /** @return \PhpParser\Node\Expr[] */
    public function unparseNodes():array {
        $nodes = [];
        foreach ($this->exprs as $expr) {
            $nodes[] = $expr->unparseExpr();
        }
        return $nodes;
    }

    public function split():array {
        return $this->exprs;
    }
}

class For_ extends ControlStructure {
    /** @var ForComma */
    private $init;
    /** @var ForComma */
    private $cond;
    /** @var ForComma */
    private $loop;
    /** @var Stmt\Block */
    private $body;

    public function __construct(CodeLoc $loc, ForComma $init, ForComma $cond, ForComma $loop, Stmt\Block $body) {
        parent::__construct($loc);
        $this->init = $init;
        $this->cond = $cond;
        $this->loop = $loop;
        $this->body = $body;
    }

    public function subStmts():array {
        return [
            $this->init,
            $this->cond,
            $this->loop,
            $this->body,
        ];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\For_(
            [
                'init'  => $this->init->unparseNodes(),
                'cond'  => $this->cond->unparseNodes(),
                'loop'  => $this->loop->unparseNodes(),
                'stmts' => $this->body->unparseNodes(),
            ]
        );
    }
}

class Switch_ extends ControlStructure {
    /** @var Expr\Expr */
    private $expr;
    /** @var SwitchCase[] */
    private $cases;

    /**
     * @param CodeLoc      $loc
     * @param Expr\Expr    $expr
     * @param SwitchCase[] $cases
     */
    public function __construct(CodeLoc $loc, Expr\Expr $expr, array $cases) {
        parent::__construct($loc);
        $this->expr  = $expr;
        $this->cases = $cases;
    }

    public function subStmts():array {
        $stmts = [$this->expr];
        foreach ($this->cases as $case) {
            foreach ($case->subStmts() as $stmt) {
                $stmts[] = $stmt;
            }
        }
        return $stmts;
    }

    public function unparseStmt() {
        $cases = [];
        foreach ($this->cases as $case) {
            $cases[] = $case->unparse();
        }
        return new \PhpParser\Node\Stmt\Switch_(
            $this->expr->unparseExpr(),
            $cases
        );
    }
}

class SwitchCase extends Node {
    /** @var Expr\Expr|null */
    private $expr;
    /** @var Stmt\Block */
    private $stmt;

    /**
     * @param CodeLoc        $loc
     * @param Expr\Expr|null $expr
     * @param Stmt\Block     $stmt
     */
    public function __construct(CodeLoc $loc, Expr\Expr $expr = null, Stmt\Block $stmt) {
        parent::__construct($loc);
        $this->expr = $expr;
        $this->stmt = $stmt;
    }

    public function subStmts():array {
        $stmts = [$this->stmt];
        if ($this->expr) {
            $stmts[] = $this->expr;
        }
        return $stmts;
    }

    public function unparse():\PhpParser\Node\Stmt\Case_ {
        return new \PhpParser\Node\Stmt\Case_(
            $this->expr ? $this->expr->unparseExpr() : null,
            $this->stmt->unparseNodes()
        );
    }
}

class While_ extends ControlStructure {
    /** @var Expr\Expr */
    private $cond;
    /** @var Stmt\Block */
    private $body;

    /**
     * @param CodeLoc    $loc
     * @param Expr\Expr  $cond
     * @param Stmt\Block $body
     */
    public function __construct(CodeLoc $loc, Expr\Expr $cond, Stmt\Block $body) {
        parent::__construct($loc);
        $this->cond = $cond;
        $this->body = $body;
    }

    public function subStmts():array {
        return [$this->cond, $this->body];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\While_(
            $this->cond->unparseExpr(),
            $this->body->unparseNodes()
        );
    }
}

class Try_ extends ControlStructure {
    /** @var Stmt\Block */
    private $body;
    /** @var TryCatch[] */
    private $catches;
    /** @var Stmt\Block */
    private $finally;

    /**
     * @param CodeLoc    $loc
     * @param Stmt\Block $body
     * @param TryCatch[] $catches
     * @param Stmt\Block $finally
     */
    public function __construct(CodeLoc $loc, Stmt\Block $body, array $catches, Stmt\Block $finally) {
        parent::__construct($loc);
        $this->body    = $body;
        $this->catches = $catches;
        $this->finally = $finally;
    }

    public function subStmts():array {
        $stmts = [$this->body];
        foreach ($this->catches as $catch) {
            foreach ($catch->subStmts() as $stmt) {
                $stmts[] = $stmt;
            }
        }
        return $stmts;
    }

    public function unparseStmt() {
        $cathes = [];
        foreach ($this->catches as $catch) {
            $cathes[] = $catch->unparse();
        }
        return new \PhpParser\Node\Stmt\TryCatch(
            $this->body->unparseNodes(),
            $cathes,
            $this->finally->unparseNodes() ?: null
        );
    }
}

class TryCatch extends Node {
    /** @var string */
    private $class;
    /** @var string */
    private $variable;
    /** @var Stmt\Block */
    private $body;

    /**
     * @param CodeLoc    $loc
     * @param string     $class
     * @param string     $variable
     * @param Stmt\Block $body
     */
    public function __construct(CodeLoc $loc, string $class, string $variable, Stmt\Block $body) {
        parent::__construct($loc);
        $this->class    = $class;
        $this->variable = $variable;
        $this->body     = $body;
    }

    public function subStmts():array {
        return [$this->body];
    }

    public function unparse():\PhpParser\Node\Stmt\Catch_ {
        return new \PhpParser\Node\Stmt\Catch_(
            new \PhpParser\Node\Name\FullyQualified($this->class),
            $this->variable,
            $this->body->unparseNodes()
        );
    }
}


