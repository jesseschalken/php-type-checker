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
        $parser = new Php7(new Emulative([
            'usedAttributes' => [
                'comments',
                'startLine',
                'endLine',
                'startFilePos',
                'endFilePos',
            ],
        ]));
        foreach ($files as $file) {
            $parsed[$file] = $parser->parse(file_get_contents($file));
        }
        $result = [];
        foreach ($parsed as $file => $nodes) {
            $result[$file] = (new self($file))->parseBlock($nodes);
        }
        return $result;
    }

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

    /** @var string */
    private $prefix = '';
    /** @var string[] */
    private $useFunction = [];
    /** @var string[] */
    private $useClass = [];
    /** @var string[] */
    private $useConst = [];

    private function __construct($file) {
        $this->__FILE__ = realpath($file);
        $this->__DIR__  = dirname($this->__FILE__);
    }

    private function resolveClass(Node\Name $name):string {
        return $this->resolve($name, $this->useClass);
    }

    private function resolveConst(Node\Name $name):string {
        return $this->resolve($name, $this->useConst);
    }

    private function resolveFunction(Node\Name $name):string {
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

    /**
     * @param Node\Stmt[] $nodes
     * @return StmtBlock
     */
    private function parseBlock(array $nodes):StmtBlock {
        /** @var SingleStmt[] $stmts */
        $stmts = [];
        $this->parseStmts($nodes, $stmts);
        return new StmtBlock($stmts);
    }

    /**
     * @param Node\Stmt[] $nodes
     * @param Stmt[]      $stmts
     * @throws \Exception
     */
    private function parseStmts(array $nodes, array &$stmts) {
        foreach ($nodes as $node) {
            if ($node instanceof Node\Expr) {
                $stmts[] = $this->parseExpr($node);
            } else if ($node instanceof Node\Stmt\If_) {
                $false = $this->parseBlock($node->else ? $node->else->stmts : []);

                foreach (array_reverse($node->elseifs) as $elseIf) {
                    $false = new StmtBlock([new If_(
                        $this->parseExpr($elseIf->cond),
                        $this->parseBlock($elseIf->stmts),
                        $false
                    )]);
                }

                $stmts[] = new If_(
                    $this->parseExpr($node->cond),
                    $this->parseBlock($node->stmts),
                    $false
                );
            } else if ($node instanceof Node\Stmt\Return_) {
                $expr    = $node->expr;
                $expr    = $expr ? $this->parseExpr($expr) : null;
                $stmts[] = new Return_($expr);
            } else if ($node instanceof Node\Stmt\Namespace_) {
                $this->parseNamespace($node, $stmts);
            } else if ($node instanceof Node\Stmt\Class_) {
                $stmts[] = new Class_();
            } else if ($node instanceof Node\Stmt\Function_) {
                // TODO
                $stmts[] = new Function_();
            } else if ($node instanceof Node\Stmt\Interface_) {
                // TODO
            } else {
                throw new \Exception('Unhandled statement type: ' . get_class($node));
            }
        }
    }

    private function parseNamespace(Node\Stmt\Namespace_ $node, array &$stmts) {
        $name   = $node->name;
        $name   = $name ? join('\\', $name->parts) . '\\' : '';
        $stmts_ = $node->stmts;

        $self              = clone $this;
        $self->prefix      = $name;
        $self->useClass    = [];
        $self->useConst    = [];
        $self->useFunction = [];

        foreach ($stmts_ as $k => $stmt) {
            // TODO handle Use here and remove from $stmts_
        }

        $self->parseStmts($stmts_, $stmts);
    }

    private function parseExpr(Node\Expr $node):Expr {
        if ($node instanceof Node\Expr\ConstFetch) {
            return new ConstFetch($this->resolveConst($node->name));
        } else if ($node instanceof Node\Expr\Assign) {
            $left  = $this->parseLValue($node->var);
            $right = $this->parseExpr($node->expr);
            return new Assign($left, $right);
        } else if ($node instanceof Node\Scalar\LNumber) {
            return new Literal($node->value);
        } else if ($node instanceof Node\Scalar\DNumber) {
            return new Literal($node->value);
        } else if ($node instanceof Node\Expr\Include_) {
            switch ($node->type) {
                case Node\Expr\Include_::TYPE_INCLUDE:
                    $require = false;
                    $once    = false;
                    break;
                case Node\Expr\Include_::TYPE_INCLUDE_ONCE:
                    $require = false;
                    $once    = true;
                    break;
                case Node\Expr\Include_::TYPE_REQUIRE:
                    $require = true;
                    $once    = false;
                    break;
                case Node\Expr\Include_::TYPE_REQUIRE_ONCE:
                    $require = true;
                    $once    = true;
                    break;
                default:
                    throw new \Exception("Unknown require type: {$node->type}");
            }

            return new Include_($this->parseExpr($node->expr), $require, $once);
        } else if ($node instanceof Node\Expr\BinaryOp\Concat) {
            return new Concat(
                $this->parseExpr($node->left),
                $this->parseExpr($node->right)
            );
        } else if ($node instanceof Node\Scalar\MagicConst) {
            $type = $node->getName();
            $line = $node->getAttribute('startLine');
            return new MagicConst($type, $this->getMagicConstValue($type, $line));
        } else if ($node instanceof Node\Scalar\String_) {
            return new Literal($node->value);
        } else if ($node instanceof Node\Expr\StaticCall) {
            $method = $node->name;
            $method = is_string($method) ? new Literal($method) : $this->parseExpr($method);

            $class = $node->class;
            $class = $class instanceof Node\Name
                ? new Literal($this->resolveClass($class))
                : $this->parseExpr($class);

            return new StaticCall($this->parseArgs($node->args), $class, $method);
        } else if ($node instanceof Node\Expr\FuncCall) {
            $function = $node->name;
            $function = $function instanceof Node\Name
                ? new Literal($this->resolveFunction($function))
                : $this->parseExpr($function);

            return new FunctionCall($function, $this->parseArgs($node->args));
        } else if ($node instanceof Node\Expr\Variable) {
            $name = $node->name;
            $name = is_string($name) ? new Literal($name) : $this->parseExpr($name);
            return new Variable($name);
        } else if ($node instanceof Node\Expr\Array_) {
            $items = [];
            foreach ($node->items as $item) {
                $key     = $item->key;
                $value   = $item->value;
                $byRef   = $item->byRef;
                $items[] = new ArrayItem(
                    $key ? $this->parseExpr($key) : null,
                    $this->parseExpr($value),
                    $byRef
                );
            }
            return new Array_($items);
        } else {
            throw new \Exception('Unhandled expression type: ' . get_class($node));
        }
    }

    /**
     * @param string $type
     * @param int    $line
     * @return string
     * @throws \Exception
     */
    private function getMagicConstValue($type, $line) {
        switch ($type) {
            case MagicConst::__LINE__:
                return $line;
            case MagicConst::__FILE__:
                return $this->__FILE__;
            case MagicConst::__DIR__:
                return $this->__DIR__;
            case MagicConst::__FUNCTION__:
                return $this->__FUNCTION__;
            case MagicConst::__CLASS__:
                return $this->__CLASS__;
            case MagicConst::__TRAIT__:
                return $this->__TRAIT__;
            case MagicConst::__METHOD__:
                return $this->__METHOD__;
            case MagicConst::__NAMESPACE__:
                return $this->__NAMESPACE__;
            default:
                throw new \Exception("Invalid magic constant type: $type");
        }
    }

    /**
     * @param Node\Arg[] $args
     * @return CallArg[]
     * @throws \Exception
     */
    private function parseArgs(array $args) {
        $result = [];
        foreach ($args as $arg) {
            $byRef    = $arg->byRef;
            $splat    = $arg->unpack;
            $expr     = $this->parseExpr($arg->value);
            $result[] = new CallArg($expr, $byRef, $splat);
        }
        return $result;
    }

    private function parseLValue(Node\Expr $expr):LValue {
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
    /** @var Stmt */
    private $true;
    /** @var Stmt */
    private $false;

    public function __construct(Expr $cond, Stmt $true, Stmt $false) {
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
     * @param string     $type
     * @param int|string $value
     */
    public function __construct($type, $value) {
        $this->type  = $type;
        $this->value = $value;
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
    public function __construct(Expr $function, array $args) {
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

    private $name;

    function name() { return $this->name; }
}

class Trait_ extends Classish {
}

class Not_ extends Expr {
    private $expr;

    function __construct(Expr $expr) {
        $this->expr = $expr;
    }
}

class Class_ extends Classish {
    private $methods = [];

    function makeAnonymous():Expr {
        return new If_(
            new Not_(new FunctionCall(new Literal('class_exists'), [
                new CallArg(new Literal($this->name()), false, false),
                new CallArg(new Literal(false), false, false),
            ])),
            new StmtBlock([$this]),
            new StmtBlock()
        );
    }
}

class Interface_ extends SingleStmt {
    private $methods = [];
}

class Function_ extends SingleStmt {
    /** @var string */
    private $name;
    /** @var bool */
    private $returnByRef;
    /** @var FunctionParam[] */
    private $params = [];
    /** @var Stmt|null */
    private $body;

    /**
     * @param string          $name
     * @param bool            $returnByRef
     * @param FunctionParam[] $params
     * @param Stmt|null       $body
     */
    public function __construct($name, $returnByRef, array $params, Stmt $body = null) {
        $this->name        = $name;
        $this->returnByRef = $returnByRef;
        $this->params      = $params;
        $this->body        = $body;
    }
}

class Method_ extends Function_ {
    /** @var string */
    private $visibility;
    /** @var bool */
    private $isStatic;
}

class FunctionParam {
    /** @var string */
    private $name;
    /** @var Expr|null */
    private $default = null;
    /** @var bool */
    private $passByRef;
    /** @var bool */
    private $variadic;

    /**
     * @param string    $name
     * @param Expr|null $default
     * @param bool      $passByRef
     * @param bool      $variadic
     */
    public function __construct($name, Expr $default = null, $passByRef, $variadic) {
        $this->name      = $name;
        $this->default   = $default;
        $this->passByRef = $passByRef;
        $this->variadic  = $variadic;
    }
}

class Array_ extends Expr {
    /** @var ArrayItem[] */
    private $items = [];

    /**
     * @param ArrayItem[] $items
     */
    public function __construct(array $items) {
        $this->items = $items;
    }
}

class ArrayItem {
    /** @var Expr|null */
    private $key;
    /** @var Expr */
    private $value;
    /** @var bool */
    private $byRef;

    /**
     * @param Expr|null $key
     * @param Expr      $value
     * @param bool      $byRef
     */
    public function __construct(Expr $key = null, Expr $value, $byRef) {
        $this->key   = $key;
        $this->value = $value;
        $this->byRef = $byRef;
    }
}

