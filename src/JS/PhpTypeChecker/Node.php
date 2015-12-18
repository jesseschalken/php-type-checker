<?php

namespace JS\PhpTypeChecker\Node;

use PhpParser\Lexer\Emulative;
use PhpParser\Node;
use PhpParser\Parser\Php7;

class Parser {
    /**
     * @param string $file
     * @return Stmt[]
     */
    static function parse($file) {
        $parser = new Php7(new Emulative());
        $parsed = $parser->parse($file);
        $stmts  = self::parseStmts($parsed, new Namespace_(''));
        return $stmts;
    }

    /**
     * @param Node\Stmt[] $nodes
     * @param Namespace_  $ns
     * @return Stmt[]
     */
    private static function parseStmts(array $nodes, Namespace_ $ns) {
        $stmts = [];
        foreach ($nodes as $node) {
            $stmts[] = self::parseStmt($node, $ns);
        }
        return $stmts;
    }

    /**
     * @param Node       $node
     * @param Namespace_ $ns
     * @return Stmt
     * @throws \Exception
     */
    private static function parseStmt(Node $node, Namespace_ $ns) {
        if ($node instanceof Node\Expr) {
            return self::parseExpr($node, $ns);
        } else if ($node instanceof Node\Stmt\If_) {
            $false = [];
            if ($node->else) {
                $false = self::parseStmts($node->else->stmts, $ns);
            }

            foreach (array_reverse($node->elseifs) as $elseIf) {
                $false = [new If_(
                    self::parseExpr($elseIf->cond, $ns),
                    self::parseStmts($elseIf->stmts, $ns),
                    $false
                )];
            }

            return new If_(
                self::parseExpr($node->cond, $ns),
                self::parseStmts($node->stmts, $ns),
                $false
            );
        } else if ($node instanceof Node\Stmt\Return_) {
            $expr = $node->expr;
            $expr = $expr ? self::parseExpr($expr, $ns) : null;
            return new Return_($expr);
        } else {
            throw new \Exception('Unhandled statement type: ' . get_class($node));
        }
    }

    /**
     * @param Node\Expr  $node
     * @param Namespace_ $ns
     * @return Expr
     * @throws \Exception
     */
    private static function parseExpr(Node\Expr $node, Namespace_ $ns) {
        if ($node instanceof Node\Expr\ConstFetch) {
            return new ConstFetch($ns->resolveConst($node->name));
        } else if ($node instanceof Node\Expr\Assign) {
            $left  = self::parseLValue($node->var, $ns);
            $right = self::parseExpr($node->expr, $ns);
            return new Assign($left, $right);
        } else if ($node instanceof Node\Scalar\LNumber) {
            return new Int_($node->value);
        } else if ($node instanceof Node\Scalar\DNumber) {
            return new Float_($node->value);
        } else {
            throw new \Exception('Unhandled expression type: ' . get_class($node));
        }
    }

    /**
     * @param Node\Expr  $expr
     * @param Namespace_ $ns
     * @return LValue
     * @throws \Exception
     */
    private static function parseLValue(Node\Expr $expr, Namespace_ $ns) {
        if ($expr instanceof Node\Expr\Variable) {
            if (is_string($expr->name)) {
                return new Variable($expr->name);
            } else {
                return new DynamicVariable(self::parseExpr($expr->name, $ns));
            }
        } else {
            throw new \Exception('Unhandled lvalue: ' . get_class($expr));
        }
    }
}

class Namespace_ {
    /** @var string */
    private $prefix;
    /** @var string[] */
    private $useFunction = [];
    /** @var string[] */
    private $useClass = [];
    /** @var string[] */
    private $useConst = [];

    /**
     * @param string $prefix
     */
    function __construct($prefix) {
        $this->prefix = $prefix;
    }

    /**
     * @param Node\Name $name
     * @return string
     */
    function resolveClass(Node\Name $name) {
        return $this->resolve($name, $this->useClass);
    }

    /**
     * @param Node\Name $name
     * @return string
     */
    function resolveConst(Node\Name $name) {
        return $this->resolve($name, $this->useConst);
    }

    /**
     * @param Node\Name $name
     * @return string
     */
    function resolveFunction(Node\Name $name) {
        return $this->resolve($name, $this->useFunction);
    }

    /**
     * @param Node\Name $name
     * @param string[]  $uses
     * @return string
     */
    private function resolve(Node\Name $name, array $uses) {
        $parts  = $name->parts;
        $class  = $parts[0];
        $suffix = count($parts) <= 1 ? '' : '\\' . join('\\', array_slice($parts, 1));

        if ($name->isFullyQualified()) {
            return $class . $suffix;
        } else if ($name->isRelative()) {
            return $this->prefix . $class . $suffix;
        } else if (isset($uses[$class])) {
            return $uses[$class] . $suffix;
        } else {
            return $this->prefix . $class . $suffix;
        }
    }
}

abstract class Stmt {
}

class If_ extends Stmt {
    /** @var Expr */
    private $cond;
    /** @var Stmt[] */
    private $true = [];
    /** @var Stmt[] */
    private $false = [];

    /**
     * @param Expr   $cond
     * @param Stmt[] $true
     * @param stmt[] $false
     */
    public function __construct($cond, array $true, array $false) {
        $this->cond  = $cond;
        $this->true  = $true;
        $this->false = $false;
    }
}

abstract class Expr extends Stmt {
}

class ConstFetch extends Expr {
    /**
     * @var string
     */
    private $name;

    /**
     * @param string $name
     */
    public function __construct($name) {
        $this->name = $name;
    }
}

class Return_ extends Stmt {
    /** @var Expr|null */
    private $expr;

    /**
     * @param Expr|null $expr
     */
    public function __construct(Expr $expr = null) {
        $this->expr = $expr;
    }
}

class Assign extends Expr {
    /** @var bool */
    private $byRef = false;
    /** @var LValue */
    private $left;
    /** @var Expr */
    private $right;

    /**
     * @param LValue $left
     * @param Expr   $right
     * @param bool   $byRef
     */
    public function __construct(LValue $left, Expr $right, $byRef = false) {
        $this->left  = $left;
        $this->right = $right;
        $this->byRef = $byRef;
    }
}

class LValue extends Expr {

}

class Variable extends LValue {
    /** @var string */
    private $name;

    /**
     * @param string $name
     */
    public function __construct($name) {
        $this->name = $name;
    }
}

class DynamicVariable extends LValue {
    /** @var Expr */
    private $expr;

    /**
     * @param Expr $expr
     */
    public function __construct(Expr $expr) {
        $this->expr = $expr;
    }
}

class Int_ extends Expr {
    /** @var int */
    private $value;

    /**
     * @param int $value
     */
    function __construct($value) {
        $this->value = $value;
    }
}

class Float_ extends Expr {
    /** @var float */
    private $value;

    /**
     * @param float $value
     */
    public function __construct($value) {
        $this->value = $value;
    }
}

class String_ extends Expr {
    /** @var string */
    private $value;

    /**
     * @param string $value
     */
    public function __construct($value) {
        $this->value = $value;
    }
}

