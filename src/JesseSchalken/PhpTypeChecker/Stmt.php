<?php

namespace JesseSchalken\PhpTypeChecker\Stmt;

use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\Expr;
use function JesseSchalken\PhpTypeChecker\extract_namespace;
use JesseSchalken\PhpTypeChecker\Node;
use function JesseSchalken\PhpTypeChecker\remove_namespace;
use JesseSchalken\PhpTypeChecker\Type;
use JesseSchalken\PhpTypeChecker\Parser;
use JesseSchalken\PhpTypeChecker\Defns;
use JesseSchalken\PhpTypeChecker\Decls;
use function JesseSchalken\MagicUtils\clone_ref;
use function JesseSchalken\PhpTypeChecker\recursive_scan2;

abstract class Stmt extends Node {
    /**
     * @return SingleStmt[]
     */
    public abstract function split():array;

    /**
     * @return Stmt[]
     */
    public abstract function subStmts():array;

    public final function namespaces():array {
        $namespaces = [];
        foreach ($this->findDeclarations() as $decl) {
            $namespaces[] = $decl->namespace_();
        }
        return array_unique($namespaces);
    }

    /**
     * @return Defns\Definition[]
     */
    public function findDeclarations():array {
        $decls = [];
        foreach ($this->subStmts() as $stmt) {
            foreach ($stmt->findDeclarations() as $decl) {
                $decls[] = $decl;
            }
        }
        return $decls;
    }
}

class Block extends Stmt {
    /** @var SingleStmt[] */
    private $stmts;

    /**
     * @param CodeLoc      $loc
     * @param SingleStmt[] $stmts
     */
    public function __construct(CodeLoc $loc, array $stmts = []) {
        parent::__construct($loc);
        $this->stmts = $stmts;
    }

    /**
     * @return \PhpParser\Node[]
     * @throws \Exception
     */
    public final function unparseWithNamespaces():array {
        $nodes = [];

        $currentNamespace = null;
        $currentNodes     = [];

        foreach ($this->stmts as $stmt) {
            $namespaces = $stmt->namespaces();
            if (count($namespaces) > 1) {
                throw new \Exception('Cant unparse single statement defining symbols in multiple namespaces');
            }

            $stmtNode      = $stmt->unparseStmt();
            $stmtNamespace = $namespaces ? $namespaces[0] : null;

            if ($stmtNode) {
                if ($stmtNamespace === null) {
                    $currentNodes[] = $stmtNode;
                } else if (
                    $currentNamespace !== null &&
                    $stmtNamespace !== $currentNamespace
                ) {
                    $nodes[] = new \PhpParser\Node\Stmt\Namespace_(
                        $currentNamespace ? new \PhpParser\Node\Name($currentNamespace) : null,
                        $currentNodes
                    );

                    $currentNamespace = $stmtNamespace;
                    $currentNodes     = [$stmtNode];
                } else {
                    $currentNamespace = $stmtNamespace;
                    $currentNodes[]   = $stmtNode;
                }
            }
        }

        if ($currentNodes) {
            $nodes[] = new \PhpParser\Node\Stmt\Namespace_(
                $currentNamespace ? new \PhpParser\Node\Name($currentNamespace) : null,
                $currentNodes
            );
        }

        if (count($nodes) == 1) {
            $node = $nodes[0];
            if ($node instanceof \PhpParser\Node\Stmt\Namespace_ && !$node->name) {
                $nodes = $node->stmts;
            }
        }

        return $nodes;
    }

    public function split():array {
        $result = [];
        foreach ($this->stmts as $stmt) {
            foreach ($stmt->split() as $stmt_) {
                $result[] = $stmt_;
            }
        }
        return $result;
    }

    public function add(SingleStmt $stmt) {
        $this->stmts[] = $stmt;
    }

    public function subStmts():array {
        return $this->stmts;
    }

    public function unparseNodes():array {
        $nodes = [];
        foreach ($this->stmts as $stmt) {
            $node = $stmt->unparseStmt();
            if ($node) {
                $nodes[] = $node;
            }
        }
        return $nodes;
    }
}

abstract class SingleStmt extends Stmt {
    public final function split():array {
        return [$this];
    }

    /**
     * @return \PhpParser\Node|null
     */
    public abstract function unparseStmt();
}

abstract class ControlStructure extends SingleStmt {
}

abstract class VariableType extends SingleStmt {
    /** @var string */
    private $name;
    /** @var Type\Type */
    private $type;

    public function __construct(CodeLoc $loc, string $name, Type\Type $type) {
        parent::__construct($loc);
        $this->name = $name;
        $this->type = $type;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseStmt() {
        return null;
    }
}

class GlobalVariableType extends VariableType {
}

class LocalVariableType extends VariableType {
}

class DoWhile extends ControlStructure {
    /** @var Block */
    private $body;
    /** @var Expr\Expr */
    private $cond;

    /**
     * @param CodeLoc   $loc
     * @param Block     $body
     * @param Expr\Expr $cond
     */
    public function __construct(CodeLoc $loc, Block $body, Expr\Expr $cond) {
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
    /** @var Block */
    private $true;
    /** @var Block */
    private $false;

    public function __construct(CodeLoc $loc, Expr\Expr $cond, Block $true, Block $false) {
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

class Return_ extends SingleStmt {
    /** @var Expr\Expr|null */
    private $expr;

    public function __construct(CodeLoc $loc, Expr\Expr $expr = null) {
        parent::__construct($loc);
        $this->expr = $expr;
    }

    public function subStmts():array {
        return $this->expr ? [$this->expr] : [];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Return_($this->expr ? $this->expr->unparseExpr() : null);
    }
}

class InlineHTML extends SingleStmt {
    /** @var string */
    private $html;

    public function __construct(CodeLoc $loc, string $html) {
        parent::__construct($loc);
        $this->html = $html;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\InlineHTML($this->html);
    }
}

class Foreach_ extends ControlStructure {
    /** @var Expr\Expr */
    private $array;
    /** @var Expr\Expr|null */
    private $key;
    /** @var Expr\Expr */
    private $value;
    /** @var Block */
    private $body;
    /** @var bool */
    private $byRef;

    /**
     * @param CodeLoc        $loc
     * @param Expr\Expr      $array
     * @param Expr\Expr|null $key
     * @param Expr\Expr      $value
     * @param bool           $byRef
     * @param Block          $body
     */
    public function __construct(
        CodeLoc $loc,
        Expr\Expr $array,
        Expr\Expr $key = null,
        Expr\Expr $value,
        bool $byRef,
        Block $body
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

class Echo_ extends SingleStmt {
    /** @var Expr\Expr[] */
    private $exprs;

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

    public function unparseStmt() {
        $exprs = [];
        foreach ($this->exprs as $expr) {
            $exprs[] = $expr->unparseExpr();
        }
        return new \PhpParser\Node\Stmt\Echo_($exprs);
    }
}

class Throw_ extends SingleStmt {
    /** @var Expr\Expr */
    private $expr;

    /**
     * @param CodeLoc   $loc
     * @param Expr\Expr $expr
     */
    public function __construct(CodeLoc $loc, Expr\Expr $expr) {
        parent::__construct($loc);
        $this->expr = $expr;
    }

    public function subStmts():array {
        return [$this->expr];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Throw_(
            $this->expr->unparseExpr()
        );
    }
}

class StaticVar extends SingleStmt {
    /** @var string */
    private $name;
    /** @var Expr\Expr|null */
    private $value;

    /**
     * @param CodeLoc        $loc
     * @param string         $name
     * @param Expr\Expr|null $value
     */
    public function __construct(CodeLoc $loc, string $name, Expr\Expr $value = null) {
        parent::__construct($loc);
        $this->name  = $name;
        $this->value = $value;
    }

    public function subStmts():array {
        return $this->value ? [$this->value] : [];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Static_(
            [
                new \PhpParser\Node\Stmt\StaticVar(
                    $this->name,
                    $this->value ? $this->value->unparseExpr() : null
                ),
            ]
        );
    }
}

/**
 * Special statements used for the init/cond/loop parts of a for loop.
 * Is like the comma operator in C and JavaScript but can actually only
 * be used in the head of a for loop.
 */
class Comma extends Stmt {
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
    /** @var Comma */
    private $init;
    /** @var Comma */
    private $cond;
    /** @var Comma */
    private $loop;
    /** @var Block */
    private $body;

    public function __construct(CodeLoc $loc, Comma $init, Comma $cond, Comma $loop, Block $body) {
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

class Break_ extends SingleStmt {
    /** @var int */
    private $levels;

    /**
     * @param CodeLoc $loc
     * @param int     $levels
     */
    public function __construct(CodeLoc $loc, int $levels = 1) {
        parent::__construct($loc);
        $this->levels = $levels;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseStmt() {
        $levels = $this->levels == 1
            ? null
            : new \PhpParser\Node\Scalar\LNumber($this->levels);
        return new \PhpParser\Node\Stmt\Break_($levels);
    }
}

class Continue_ extends SingleStmt {
    /** @var int */
    private $levels;

    /**
     * @param CodeLoc $loc
     * @param int     $levels
     */
    public function __construct(CodeLoc $loc, int $levels) {
        parent::__construct($loc);
        $this->levels = $levels;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseStmt() {
        $levels = $this->levels == 1
            ? null
            : new \PhpParser\Node\Scalar\LNumber($this->levels);
        return new \PhpParser\Node\Stmt\Continue_($levels);
    }
}

class Switch_ extends ControlStructure {
    /** @var Expr\Expr */
    private $expr;
    /** @var Case_[] */
    private $cases;

    /**
     * @param CodeLoc   $loc
     * @param Expr\Expr $expr
     * @param Case_[]   $cases
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

class Case_ extends Node {
    /** @var Expr\Expr|null */
    private $expr;
    /** @var Block */
    private $stmt;

    /**
     * @param CodeLoc        $loc
     * @param Expr\Expr|null $expr
     * @param Block          $stmt
     */
    public function __construct(CodeLoc $loc, Expr\Expr $expr = null, Block $stmt) {
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

class Unset_ extends SingleStmt {
    /** @var Expr\Expr[] */
    private $exprs;

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

    public function unparseStmt() {
        $exprs = [];
        foreach ($this->exprs as $expr) {
            $exprs[] = $expr->unparseExpr();
        }
        return new \PhpParser\Node\Stmt\Unset_($exprs);
    }
}

class While_ extends ControlStructure {
    /** @var Expr\Expr */
    private $cond;
    /** @var Block */
    private $body;

    /**
     * @param CodeLoc   $loc
     * @param Expr\Expr $cond
     * @param Block     $body
     */
    public function __construct(CodeLoc $loc, Expr\Expr $cond, Block $body) {
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
    /** @var Block */
    private $body;
    /** @var Catch_[] */
    private $catches;
    /** @var Block */
    private $finally;

    /**
     * @param CodeLoc  $loc
     * @param Block    $body
     * @param Catch_[] $catches
     * @param Block    $finally
     */
    public function __construct(CodeLoc $loc, Block $body, array $catches, Block $finally) {
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

class Catch_ extends Node {
    /** @var string */
    private $class;
    /** @var string */
    private $variable;
    /** @var Block */
    private $body;

    /**
     * @param CodeLoc $loc
     * @param string  $class
     * @param string  $variable
     * @param Block   $body
     */
    public function __construct(CodeLoc $loc, string $class, string $variable, Block $body) {
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

class Global_ extends SingleStmt {
    /** @var Expr\Expr */
    private $expr;

    /**
     * @param CodeLoc   $loc
     * @param Expr\Expr $expr
     */
    public function __construct(CodeLoc $loc, Expr\Expr $expr) {
        parent::__construct($loc);
        $this->expr = $expr;
    }

    public function subStmts():array {
        return [$this->expr];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Global_(
            [
                $this->expr->unparseExpr(),
            ]
        );
    }
}

class Label_ extends SingleStmt {
    /** @var string */
    private $name;

    public function __construct(CodeLoc $loc, string $name) {
        parent::__construct($loc);
        $this->name = $name;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Label($this->name);
    }
}

class Goto_ extends SingleStmt {
    /** @var string */
    private $name;

    public function __construct(CodeLoc $loc, string $name) {
        parent::__construct($loc);
        $this->name = $name;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Goto_($this->name);
    }
}
