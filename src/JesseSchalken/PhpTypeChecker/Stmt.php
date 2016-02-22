<?php

namespace JesseSchalken\PhpTypeChecker\Stmt;

use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\Context;
use JesseSchalken\PhpTypeChecker\Defns;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Parser;
use JesseSchalken\PhpTypeChecker\Type;
use function JesseSchalken\MagicUtils\clone_ref;
use function JesseSchalken\PhpTypeChecker\extract_namespace;
use function JesseSchalken\PhpTypeChecker\recursive_scan2;
use function JesseSchalken\PhpTypeChecker\remove_namespace;
use function JesseSchalken\PhpTypeChecker\merge_types;

abstract class Stmt extends Node {
    /**
     * @return SingleStmt[]
     */
    public abstract function split():array;

    /**
     * @param bool $deep If true, also return statements/expressions that belong to different scopes (eg, inside a
     *                   function/class definition). If false, only return statements/expressions that are evaluated in
     *                   the same scope.
     * @return Stmt[]
     */
    public abstract function subStmts(bool $deep):array;

    public final function gatherInferedLocals(Context\Context $context) {
        $declared = $context->getLocals();
        do {
            $added = false;
            $types = $this->inferLocals($context);
            foreach ($types as $name => $type) {
                // Ignore variables that were already declared at the outset
                if (isset($declared[$name])) {
                    continue;
                }
                $type2 = $context->getLocal($name);
                if (!$type2) {
                    // Add the variable if it doesn't exist
                    $context->addLocal($name, $type);
                    $added = true;
                } else if (!$type2->containsType($type, $context)) {
                    // Merge it in if we have discovered new types for it
                    $context->addLocal($name, $type->addType($type2, $context));
                    $added = true;
                }
            }
        } while ($added);
    }

    /**
     * Infer local variables based on direct assignments of the form "$foo = ...". "$context" will already have local
     * variables based on explicit "@var" doc comments, and parameters.
     * @param Context\Context $context
     * @return Type\Type[]
     */
    private function inferLocals(Context\Context $context):array {
        $types = $this->inferLocals_($context);
        foreach ($this->subStmts(false) as $stmt) {
            $types = merge_types($stmt->inferLocals($context), $types, $context);
        }
        return $types;
    }

    /**
     * @param Context\Context $context
     * @return Type\Type[]
     */
    protected function inferLocals_(Context\Context $context):array {
        return [];
    }

    public final function namespaces():array {
        $namespaces = [];
        foreach ($this->subStmts(true) as $stmt) {
            if ($stmt instanceof Defns\HasNamespace) {
                $namespaces[] = $stmt->namespace_();
            }
        }
        return array_unique($namespaces);
    }

    public function gatherGlobalDecls(Context\Context $context) {
        foreach ($this->subStmts(true) as $stmt) {
            $stmt->gatherGlobalDecls($context);
        }
    }

    public function gatherLocalDecls(Context\Context $context) {
        foreach ($this->subStmts(false) as $stmt) {
            $stmt->gatherLocalDecls($context);
        }
    }

    public function checkStmt(Context\Context $context) {
        foreach ($this->subStmts(true) as $stmt) {
            $stmt->checkStmt($context);
        }
    }
}

class Block extends Stmt {
    /** @var SingleStmt[] */
    private $stmts;

    /**
     * @param HasCodeLoc   $loc
     * @param SingleStmt[] $stmts
     */
    public function __construct(HasCodeLoc $loc, array $stmts = []) {
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

    public function subStmts(bool $deep):array {
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

/**
 * A statement that isn't a block.
 */
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

    public function __construct(HasCodeLoc $loc, Expr\Expr $expr = null) {
        parent::__construct($loc);
        $this->expr = $expr;
    }

    public function subStmts(bool $deep):array {
        return $this->expr ? [$this->expr] : [];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Return_($this->expr ? $this->expr->unparseExpr() : null);
    }
}

class InlineHTML extends SingleStmt {
    /** @var string */
    private $html;

    public function __construct(HasCodeLoc $loc, string $html) {
        parent::__construct($loc);
        $this->html = $html;
    }

    public function subStmts(bool $deep):array {
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
     * @param HasCodeLoc  $loc
     * @param Expr\Expr[] $exprs
     */
    public function __construct(HasCodeLoc $loc, array $exprs) {
        parent::__construct($loc);
        $this->exprs = $exprs;
    }

    public function subStmts(bool $deep):array {
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
     * @param HasCodeLoc $loc
     * @param Expr\Expr  $expr
     */
    public function __construct(HasCodeLoc $loc, Expr\Expr $expr) {
        parent::__construct($loc);
        $this->expr = $expr;
    }

    public function subStmts(bool $deep):array {
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
     * @param HasCodeLoc     $loc
     * @param string         $name
     * @param Expr\Expr|null $value
     */
    public function __construct(HasCodeLoc $loc, string $name, Expr\Expr $value = null) {
        parent::__construct($loc);
        $this->name  = $name;
        $this->value = $value;
    }

    public function subStmts(bool $deep):array {
        return $this->value ? [$this->value] : [];
    }

    protected function inferLocals_(Context\Context $context):array {
        $value = $this->value;
        if ($value) {
            return [$this->name => $value->checkExpr($context, true)];
        } else {
            return [];
        }
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Static_([
            new \PhpParser\Node\Stmt\StaticVar(
                $this->name,
                $this->value ? $this->value->unparseExpr() : null
            ),
        ]);
    }
}

class Break_ extends SingleStmt {
    /** @var int */
    private $levels;

    /**
     * @param HasCodeLoc $loc
     * @param int        $levels
     */
    public function __construct(HasCodeLoc $loc, int $levels = 1) {
        parent::__construct($loc);
        $this->levels = $levels;
    }

    public function subStmts(bool $deep):array {
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
     * @param HasCodeLoc $loc
     * @param int        $levels
     */
    public function __construct(HasCodeLoc $loc, int $levels) {
        parent::__construct($loc);
        $this->levels = $levels;
    }

    public function subStmts(bool $deep):array {
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
     * @param HasCodeLoc  $loc
     * @param Expr\Expr[] $exprs
     */
    public function __construct(HasCodeLoc $loc, array $exprs) {
        parent::__construct($loc);
        $this->exprs = $exprs;
    }

    public function subStmts(bool $deep):array {
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
     * @param HasCodeLoc $loc
     * @param Expr\Expr  $expr
     */
    public function __construct(HasCodeLoc $loc, Expr\Expr $expr) {
        parent::__construct($loc);
        $this->expr = $expr;
    }

    public function subStmts(bool $deep):array {
        return [$this->expr];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Global_([
            $this->expr->unparseExpr(),
        ]);
    }

    protected function inferLocals_(Context\Context $context):array {
        return $this->expr->checkExpr($context, true)->useToInferGlobal($context);
    }
}

class Goto_ extends SingleStmt {
    /** @var string */
    private $name;

    public function __construct(HasCodeLoc $loc, string $name) {
        parent::__construct($loc);
        $this->name = $name;
    }

    public function subStmts(bool $deep):array {
        return [];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Goto_($this->name);
    }

    public function checkStmt(Context\Context $context) {
        if (!$context->hasLabel($this->name)) {
            $context->addError("Undefined label '$this->name'", $this);
        }
        parent::checkStmt($context);
    }
}
