<?php

namespace JesseSchalken\PhpTypeChecker\Node;

use PhpParser\Lexer\Emulative;
use PhpParser\Node;
use PhpParser\Parser\Php7;

class Parser {
    /**
     * @param string[] $files
     * @return StmtBlock[]
     */
    static function parseFiles(array $files):array {
        /** @var Node[][] $parsed */
        $parsed = [];
        $parser = new Php7(new Emulative());
        foreach ($files as $file) {
            $parsed[$file] = $parser->parse(file_get_contents($file));
        }
        $result = [];
        foreach ($parsed as $file => $nodes) {
            $result[$file] = self::parseBlock($nodes, new Namespace_(''));
        }
        return $result;
    }

    /**
     * @param Node\Stmt[] $nodes
     * @param Namespace_  $ns
     * @return StmtBlock
     */
    private static function parseBlock(array $nodes, Namespace_ $ns):StmtBlock {
        /** @var SingleStmt[] $stmts */
        $stmts = [];
        self::parseStmts($nodes, $ns, $stmts);
        return new StmtBlock($stmts);
    }

    /**
     * @param Node\Stmt[] $nodes
     * @param Namespace_  $ns
     * @param Stmt[]      $stmts
     * @throws \Exception
     */
    private static function parseStmts(array $nodes, Namespace_ $ns, array &$stmts) {
        foreach ($nodes as $node) {
            if ($node instanceof Node\Expr) {
                $stmts[] = self::parseExpr($node, $ns);
            } else if ($node instanceof Node\Stmt\If_) {
                $false = self::parseBlock($node->else ? $node->else->stmts : [], $ns);

                foreach (array_reverse($node->elseifs) as $elseIf) {
                    $false = new StmtBlock([new If_(
                        self::parseExpr($elseIf->cond, $ns),
                        self::parseBlock($elseIf->stmts, $ns),
                        $false
                    )]);
                }

                $stmts[] = new If_(
                    self::parseExpr($node->cond, $ns),
                    self::parseBlock($node->stmts, $ns),
                    $false
                );
            } else if ($node instanceof Node\Stmt\Return_) {
                $expr    = $node->expr;
                $expr    = $expr ? self::parseExpr($expr, $ns) : null;
                $stmts[] = new Return_($expr);
            } else if ($node instanceof Node\Stmt\Namespace_) {
                self::parseNamespace($node, $stmts);
            } else if ($node instanceof Node\Stmt\Class_) {
                $stmts[] = new Class_();
            } else if ($node instanceof Node\Stmt\Function_) {
                $stmts[] = new Function_();
            } else {
                throw new \Exception('Unhandled statement type: ' . get_class($node));
            }
        }
    }

    private static function parseNamespace(Node\Stmt\Namespace_ $node, array &$stmts) {
        $name   = $node->name;
        $name   = $name ? join('\\', $name->parts) . '\\' : '';
        $stmts_ = $node->stmts;
        $ns     = new Namespace_($name);
        foreach ($stmts_ as $k => $stmt) {
            // TODO handle Use here and remove from $stmts_
        }

        self::parseStmts($stmts_, $ns, $stmts);
    }

    private static function parseExpr(Node\Expr $node, Namespace_ $ns):Expr {
        if ($node instanceof Node\Expr\ConstFetch) {
            return new ConstFetch($ns->resolveConst($node->name));
        } else if ($node instanceof Node\Expr\Assign) {
            $left  = self::parseLValue($node->var, $ns);
            $right = self::parseExpr($node->expr, $ns);
            return new Assign($left, $right);
        } else if ($node instanceof Node\Scalar\LNumber) {
            return new Literal($node->value);
        } else if ($node instanceof Node\Scalar\DNumber) {
            return new Literal($node->value);
        } else if ($node instanceof Node\Expr\Include_) {
            switch ($node->type) {
                case Node\Expr\Include_::TYPE_INCLUDE:
                case Node\Expr\Include_::TYPE_INCLUDE_ONCE:
                    $require = false;
                    break;
                case Node\Expr\Include_::TYPE_REQUIRE:
                case Node\Expr\Include_::TYPE_REQUIRE_ONCE:
                    $require = true;
                    break;
                default:
                    throw new \Exception("Unknown require type: {$node->type}");
            }
            switch ($node->type) {
                case Node\Expr\Include_::TYPE_INCLUDE:
                case Node\Expr\Include_::TYPE_REQUIRE:
                    $once = false;
                    break;
                case Node\Expr\Include_::TYPE_INCLUDE_ONCE:
                case Node\Expr\Include_::TYPE_REQUIRE_ONCE:
                    $once = true;
                    break;
                default:
                    throw new \Exception("Unknown require type: {$node->type}");
            }

            return new Include_(self::parseExpr($node->expr, $ns), $require, $once);
        } else if ($node instanceof Node\Expr\BinaryOp\Concat) {
            return new Concat(
                self::parseExpr($node->left, $ns),
                self::parseExpr($node->right, $ns)
            );
        } else if ($node instanceof Node\Scalar\MagicConst) {
            return new MagicConst($node->getName());
        } else if ($node instanceof Node\Scalar\String_) {
            return new Literal($node->value);
        } else if ($node instanceof Node\Expr\StaticCall) {
            $method = $node->name;
            $class  = $node->class;
            if (is_string($method)) {
                $method = new Literal($method);
            }
            if ($class instanceof Node\Name) {
                $class = new Literal($ns->resolveClass($class));
            }
            return new StaticCall(self::parseArgs($node->args, $ns), $class, $method);
        } else {
            throw new \Exception('Unhandled expression type: ' . get_class($node));
        }
    }

    /**
     * @param Node\Arg[] $args
     * @param Namespace_ $ns
     * @return CallArg[]
     * @throws \Exception
     */
    private static function parseArgs(array $args, Namespace_ $ns) {
        $result = [];
        foreach ($args as $arg) {
            $byRef    = $arg->byRef;
            $splat    = $arg->unpack;
            $expr     = self::parseExpr($arg->value, $ns);
            $result[] = new CallArg($expr, $byRef, $splat);
        }
        return $result;
    }

    private static function parseLValue(Node\Expr $expr, Namespace_ $ns):LValue {
        if ($expr instanceof Node\Expr\Variable) {
            $name = $expr->name;
            if (is_string($name)) {
                $name = new Literal($name);
            }
            return new Variable($name);
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

    function resolveClass(Node\Name $name):string {
        return $this->resolve($name, $this->useClass);
    }

    function resolveConst(Node\Name $name):string {
        return $this->resolve($name, $this->useConst);
    }

    function resolveFunction(Node\Name $name):string {
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

final class StmtBlock extends Stmt {
    /** @var SingleStmt[] */
    private $stmts;

    /**
     * @param SingleStmt[] $stmts
     */
    public function __construct(array $stmts = []) {
        $this->stmts = $stmts;
    }

    public function stmts() {
        return $this->stmts;
    }
}

abstract class SingleStmt extends Stmt {
}

class If_ extends SingleStmt {
    /** @var Expr */
    private $cond;
    /** @var StmtBlock */
    private $true;
    /** @var StmtBlock */
    private $false;

    public function __construct(Expr $cond, StmtBlock $true, StmtBlock $false) {
        $this->cond  = $cond;
        $this->true  = $true;
        $this->false = $false;
    }
}

abstract class Expr extends SingleStmt {
}

class ConstFetch extends SingleStmt {
    /** @var string */
    private $name;

    public function __construct(string $name) {
        $this->name = $name;
    }
}

class Return_ extends SingleStmt {
    /** @var Expr|null */
    private $expr;

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

    public function __construct(LValue $left, Expr $right, bool $byRef = false) {
        $this->left  = $left;
        $this->right = $right;
        $this->byRef = $byRef;
    }
}

class LValue extends Expr {

}

class Variable extends LValue {
    /** @var Expr */
    private $name;

    public function __construct(Expr $name) {
        $this->name = $name;
    }
}

class DynamicVariable extends LValue {
}

class Include_ extends Expr {
    private $require = true;
    private $once    = true;
    /** @var Expr */
    private $expr;

    /**
     * @param Expr $expr
     * @param bool $require
     * @param bool $once
     */
    public function __construct(Expr $expr, $require, $once) {
        $this->require = $require;
        $this->once    = $once;
        $this->expr    = $expr;
    }
}

class Concat extends Expr {
    /** @var  Expr */
    private $left;
    /** @var  Expr */
    private $right;

    /**
     * @param Expr $left
     * @param Expr $right
     */
    public function __construct(Expr $left, Expr $right) {
        $this->left  = $left;
        $this->right = $right;
    }
}

class MagicConst extends Expr {
    const __LINE__      = '__LINE__';
    const __FILE__      = '__FILE__';
    const __DIR__       = '__DIR__';
    const __FUNCTION__  = '__FUNCTION__';
    const __CLASS__     = '__CLASS__';
    const __TRAIT__     = '__TRAIT__';
    const __METHOD__    = '__METHOD__';
    const __NAMESPACE__ = '__NAMESPACE__';

    /** @var string */
    private $type;
    /** @var int|string */
    private $value;

    /**
     * @param string $type
     * @param MagicConstants $magic
     * @param int $line
     */
    public function __construct($type, MagicConstants $magic, $line) {
        $this->type  = $type;
        $this->value = $this->getValue($magic, $line);
    }

    /**
     * @param MagicConstants $magic
     * @param int $line
     */
    private function getValue(MagicConstants $magic, $line) {
        switch ($this->type) {
            case self::__LINE__: return $line;
            case self::__FILE__: return $magic->__FILE__;
            case self::__DIR__: return $magic->__DIR__;
            case self::__FUNCTION__: return $magic->__FUNCTION__;
            case self::__CLASS__: return $magic->__CLASS__;
            case self::__TRAIT__: return $magic->__TRAIT__;
            case self::__METHOD__: return $magic->__METHOD__;
            case self::__NAMESPACE__: return $magic->__NAMESPACE__;
            default: throw new \Exception("Invalid magic constant type: $this->type");
        }
    }
}

class MagicConstants {
    /** @var string */ 
    public $__DIR__ = '';
    /** @var string */ 
    public $__FILE__ = '';
    /** @var int */ 
    // You should use the line number from the parser node
    // public $__LINE__ = '';
    /** @var string */ 
    public $__FUNCTION__ = '';
    /** @var string */ 
    public $__CLASS__ = '';
    /** @var string */ 
    public $__TRAIT__ = '';
    /** @var string */ 
    public $__METHOD__ = '';
    /** @var string */ 
    public $__NAMESPACE__ = '';

    /**
     * @param string $file
     */
    public function __construct($file) {
        $this->__FILE__ = realpath($file);
        $this->__DIR__  = dirname($this->__FILE__);
    }
}

class Literal extends Expr {
    /** @var array|bool|float|int|null|string */
    private $value;

    /**
     * @param string|int|float|bool|null|array $value
     */
    public function __construct($value) {
        $this->value = $value;
    }
}

class Call extends Expr {
    /** @var CallArg[] */
    private $args = [];

    /**
     * @param CallArg[] $args
     */
    public function __construct(array $args) {
        $this->args = $args;
    }
}

class FunctionCall extends Call {
    /** @var Expr */
    private $function;

    /**
     * @param CallArg[] $args
     * @param Expr      $function
     */
    public function __construct(array $args, Expr $function) {
        parent::__construct($args);
        $this->function = $function;
    }
}

class StaticCall extends Call {
    /** @var Expr */
    private $class;
    /** @var Expr */
    private $method;

    /**
     * @param CallArg[] $args
     * @param Expr      $class
     * @param Expr      $method
     */
    public function __construct(array $args, Expr $class, Expr $method) {
        parent::__construct($args);
        $this->class  = $class;
        $this->method = $method;
    }
}

class MethodCall extends Call {
    /** @var Expr */
    private $object;
    /** @var Expr */
    private $method;

    /**
     * @param CallArg[] $args
     * @param Expr      $object
     * @param Expr      $method
     */
    public function __construct(array $args, Expr $object, Expr $method) {
        parent::__construct($args);
        $this->object = $object;
        $this->method = $method;
    }
}

class CallArg {
    /** @var Expr */
    private $expr;
    private $byRef = false;
    private $splat = false;

    /**
     * @param Expr $expr
     * @param bool $byRef
     * @param bool $splat
     */
    public function __construct(Expr $expr, $byRef, $splat) {
        $this->expr  = $expr;
        $this->byRef = $byRef;
        $this->splat = $splat;
    }
}

/**
 * static::name
 */
class Static_ extends Expr {
}

class Classish extends SingleStmt {
    const PUBLIC    = 'public';
    const PROTECTED = 'protected';
    const PRIVATE   = 'private';
}

class Trait_ extends Classish {
}

class Class_ extends Classish {
    private $methods;

}

class Interface_ extends SingleStmt {
    private $methods = [];
}

class Function_ extends SingleStmt {
    private $name;
}

class FunctionParam {

}

