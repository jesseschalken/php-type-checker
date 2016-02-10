<?php

namespace JesseSchalken\PhpTypeChecker\Stmt;

use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\Decls;
use JesseSchalken\PhpTypeChecker\Defns;
use JesseSchalken\PhpTypeChecker\ErrorReceiver;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Parser;
use JesseSchalken\PhpTypeChecker\Type;
use function JesseSchalken\MagicUtils\clone_ref;
use function JesseSchalken\PhpTypeChecker\extract_namespace;
use function JesseSchalken\PhpTypeChecker\recursive_scan2;
use function JesseSchalken\PhpTypeChecker\remove_namespace;

abstract class Stmt extends Node {
    /**
     * @return SingleStmt[]
     */
    public abstract function split():array;

    /**
     * @return Stmt[]
     */
    public abstract function subStmts():array;

    /**
     * Same as subStmts() but doesn't descend into class/function definitions
     * @return Stmt[]
     */
    public function localSubStmts():array {
        return $this->subStmts();
    }

    public final function namespaces():array {
        $namespaces = [];
        foreach ($this->findDefinitions() as $decl) {
            if ($decl instanceof Defns\HasNamespace) {
                $namespaces[] = $decl->namespace_();
            }
        }
        return array_unique($namespaces);
    }

    /**
     * @return Defns\Definition[]
     */
    public function findDefinitions():array {
        $decls = [];
        foreach ($this->subStmts() as $stmt) {
            foreach ($stmt->findDefinitions() as $decl) {
                $decls[] = $decl;
            }
        }
        return $decls;
    }

    public function gatherGlobalDecls(Decls\GlobalDecls $decls) {
        foreach ($this->subStmts() as $stmt) {
            $stmt->gatherGlobalDecls($decls);
        }
    }

    public function gatherLocalDecls(Decls\LocalDecls $decls) {
        foreach ($this->localSubStmts() as $stmt) {
            $stmt->gatherLocalDecls($decls);
        }
    }

    public function typeCheck(Decls\GlobalDecls $globals, Decls\LocalDecls $locals, ErrorReceiver $errors) {
        foreach ($this->subStmts() as $stmt) {
            $stmt->typeCheck($globals, $locals, $errors);
        }
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
