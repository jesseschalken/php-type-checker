<?php

namespace JesseSchalken\PhpTypeChecker\Node;

use JesseSchalken\MagicUtils\DeepClone;
use PhpParser\Lexer;
use PhpParser\Node;
use PhpParser\Parser\Php7;

class Parser {
    /**
     * @param string[] $files
     * @return StmtBlock[]
     */
    static function parseFiles(array $files):array {
        $result = [];
        foreach (self::parseFilesNodes($files) as $file => $nodes) {
            $result[$file] = (new self($file))->parseStmts($nodes);
        }
        return $result;
    }

    private static function parseFilesNodes(array $files) {
        $parser = new Php7(
            new Lexer([
                'usedAttributes' => [
                    'comments',
                    'startLine',
                    'endLine',
                    'startFilePos',
                    'endFilePos',
                ],
            ]),
            [
                'throwOnError' => false,
            ]
        );

        foreach ($files as $file) {
            yield $file => $parser->parse(file_get_contents($file));
            foreach ($parser->getErrors() as $error) {
                print "$file {$error->getMessage()}\n";
            }
        }
    }

    use DeepClone;

    /** @var string */
    public $__DIR__ = '';
    /** @var string */
    public $__FILE__ = '';
    /**
     * @deprecated You should use the line number from the parser node
     * @var int
     */
    public $__LINE__ = '';
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
        $this->setFile($file);
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
    private function parseStmts(array $nodes):StmtBlock {
        /** @var SingleStmt[] $stmts */
        $stmts = [];
        foreach ($nodes as $node) {
            $stmt = $this->parseStmt($node);
            if ($stmt instanceof SingleStmt) {
                $stmts[] = $stmt;
            } else {
                foreach ($stmt->stmts() as $stmt) {
                    $stmts[] = $stmt;
                }
            }
        }
        return new StmtBlock($stmts);
    }

    private function parseStmt(Node $node):Stmt {
        if ($node instanceof Node\Expr) {
            return $this->parseExpr($node);
        } else if ($node instanceof Node\Stmt\If_) {
            $false = $this->parseStmts($node->else ? $node->else->stmts : []);

            foreach (array_reverse($node->elseifs) as $elseIf) {
                $false = new If_(
                    $this->parseExpr($elseIf->cond),
                    $this->parseStmts($elseIf->stmts),
                    $false
                );
            }

            return new If_(
                $this->parseExpr($node->cond),
                $this->parseStmts($node->stmts),
                $false
            );
        } else if ($node instanceof Node\Stmt\Return_) {
            return new Return_($this->parseExprNull($node->expr));
        } else if ($node instanceof Node\Stmt\Namespace_) {
            $copy              = clone $this;
            $copy->prefix      = $node->name ? $node->name->toString() . '\\' : '';
            $copy->useClass    = [];
            $copy->useConst    = [];
            $copy->useFunction = [];

            return $copy->parseStmts($node->stmts);
        } else if ($node instanceof Node\Stmt\Class_) {
            return new Class_(
                $this->prefix . $node->name,
                $this->parseClassMembers($node)
            );
        } else if ($node instanceof Node\Stmt\Function_) {
            return new Function_(
                $this->prefix . $node->name,
                $this->parseFunctionType($node),
                $this->parseStmts($node->stmts)
            );
        } else if ($node instanceof Node\Stmt\Interface_) {
            return new Interface_(
                $this->prefix . $node->name,
                $this->parseClassMembers($node)
            );
        } else if ($node instanceof Node\Stmt\Trait_) {
            return new Trait_(
                $this->prefix . $node->name,
                $this->parseClassMembers($node)
            );
        } else if ($node instanceof Node\Stmt\Use_) {
            $this->addUses($node->uses, $node->type);
            return new StmtBlock();
        } else if ($node instanceof Node\Stmt\GroupUse) {
            $prefix = $node->prefix->toString() . '\\';
            if ($prefix === '\\')
                $prefix = '';
            $this->addUses($node->uses, $node->type, $prefix);
            return new StmtBlock();
        } else if ($node instanceof Node\Stmt\Foreach_) {
            return new Foreach_(
                $this->parseExpr($node->expr),
                $this->parseExprNull($node->keyVar),
                $this->parseExpr($node->valueVar),
                $node->byRef,
                $this->parseStmts($node->stmts)
            );
        } else if ($node instanceof Node\Stmt\Echo_) {
            return new Echo_($this->parseExprs($node->exprs));
        } else if ($node instanceof Node\Stmt\InlineHTML) {
            return new InlineHTML($node->value);
        } else if ($node instanceof Node\Stmt\Const_) {
            $stmts = [];
            foreach ($node->consts as $const) {
                $stmts[] = new Const_(
                    $this->prefix . $const->name,
                    $this->parseExpr($const->value)
                );
            }
            return new StmtBlock($stmts);
        } else if ($node instanceof Node\Stmt\Throw_) {
            return new Throw_($this->parseExpr($node->expr));
        } else if ($node instanceof Node\Stmt\Static_) {
            $stmts = [];
            foreach ($node->vars as $var) {
                $stmts[] = new StaticVar(
                    $var->name,
                    $this->parseExprNull($var->default)
                );
            }
            return new StmtBlock($stmts);
        } else if ($node instanceof Node\Stmt\For_) {
            return new For_(
                $this->parseExprs($node->init),
                $this->parseExprs($node->cond),
                $this->parseExprs($node->loop),
                $this->parseStmts($node->stmts)
            );
        } else if ($node instanceof Node\Stmt\Break_) {
            if ($node->num === null) {
                $levels = 1;
            } else if ($node->num instanceof Node\Scalar\LNumber) {
                $levels = $node->num->value;
            } else {
                throw new \Exception('"break" statement must use a constant operand');
            }

            return new Break_($levels);
        } else if ($node instanceof Node\Stmt\Continue_) {
            if ($node->num === null) {
                $levels = 1;
            } else if ($node->num instanceof Node\Scalar\LNumber) {
                $levels = $node->num->value;
            } else {
                throw new \Exception('"continue" statement must use a constant operand');
            }

            return new Continue_($levels);
        } else if ($node instanceof Node\Stmt\Switch_) {
            $cases = [];
            foreach ($node->cases as $case) {
                $cases[] = new Case_(
                    $this->parseExprNull($case->cond),
                    $this->parseStmts($case->stmts)
                );
            }
            return new Switch_(
                $this->parseExpr($node->cond),
                $cases
            );
        } else if ($node instanceof Node\Stmt\Unset_) {
            return new Unset_($this->parseExprs($node->vars));
        } else if ($node instanceof Node\Stmt\While_) {
            return new While_($this->parseExpr($node->cond), $this->parseStmts($node->stmts));
        } else {
            throw new \Exception('Unhandled statement type: ' . get_class($node));
        }
    }

    /**
     * @param Node\Stmt\ClassLike $node
     * @return ClassMember[]
     */
    private function parseClassMembers(Node\Stmt\ClassLike $node) {
        foreach ($node->stmts as $stmt) {
            if ($stmt instanceof Node\Stmt\ClassMethod && $stmt->stmts) {
                $this->parseStmts($stmt->stmts);
            }
        }
        return [];
    }

    /**
     * @param Node\FunctionLike $node
     * @return FunctionSignature
     */
    private function parseFunctionType(Node\FunctionLike $node) {
        return new FunctionSignature(false, []);
    }

    /**
     * @param Node\Stmt\UseUse[] $uses
     * @param int                $type_
     * @param string             $prefix
     * @throws \Exception
     */
    private function addUses(array $uses, $type_, $prefix = '') {
        foreach ($uses as $use) {
            $name  = $prefix . $use->name->toString();
            $alias = $use->alias === null ? $use->name->getLast() : $use->alias;
            $type  = $use->type === Node\Stmt\Use_::TYPE_UNKNOWN ? $type_ : $use->type;
            switch ($type) {
                case Node\Stmt\Use_::TYPE_CONSTANT:
                    $this->useConst[$alias] = $name;
                    break;
                case Node\Stmt\Use_::TYPE_FUNCTION:
                    $this->useFunction[$alias] = $name;
                    break;
                case Node\Stmt\Use_::TYPE_NORMAL:
                    $this->useClass[$alias] = $name;
                    break;
                default:
                    throw new \Exception('Invalid use type: ' . $type);
            }
        }
    }

    /**
     * @param Node\Expr[] $nodes
     * @return Expr[]
     */
    private function parseExprs(array $nodes):array {
        $exprs = [];
        foreach ($nodes as $k => $v) {
            $exprs[] = $this->parseExpr($v);
        }
        return $exprs;
    }

    /**
     * @param Node\Expr|null $node
     * @return Expr|null
     */
    private function parseExprNull(Node\Expr $node = null) {
        return $node ? $this->parseExpr($node) : null;
    }

    /**
     * @param Node\Expr|string $node
     * @return Expr
     */
    private function parseExprString($node) {
        return is_string($node) ? new Literal($node) : $this->parseExpr($node);
    }

    /**
     * @param Node\Expr|Node\Name $node
     * @return Expr
     * @throws \Exception
     */
    private function parseExprClass($node) {
        return $node instanceof Node\Name
            ? new Literal($this->resolveClass($node))
            : $this->parseExpr($node);
    }

    private function parseExpr(Node\Expr $node):Expr {
        if ($node instanceof Node\Expr\Variable) {
            return new Variable($this->parseExprString($node->name));
        } else if ($node instanceof Node\Expr\ConstFetch) {
            return new ConstFetch($this->resolveConst($node->name));
        } else if ($node instanceof Node\Expr\Assign) {
            return new BinOp(
                $this->parseExpr($node->var),
                BinOp::ASSIGN,
                $this->parseExpr($node->expr)
            );
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
            return new BinOp(
                $this->parseExpr($node->left),
                BinOp::CONCAT,
                $this->parseExpr($node->right)
            );
        } else if ($node instanceof Node\Scalar\MagicConst) {
            $type = $node->getName();
            $line = $node->getAttribute('startLine');
            return new MagicConst($type, $this->getMagicConstValue($type, $line));
        } else if ($node instanceof Node\Scalar\String_) {
            return new Literal($node->value);
        } else if ($node instanceof Node\Expr\StaticCall) {
            return new StaticCall(
                $this->parseArgs($node->args),
                $this->parseExprClass($node->class),
                $this->parseExprString($node->name)
            );
        } else if ($node instanceof Node\Expr\FuncCall) {
            $function = $node->name;
            $function = $function instanceof Node\Name
                ? new Literal($this->resolveFunction($function))
                : $this->parseExpr($function);

            return new FunctionCall($function, $this->parseArgs($node->args));
        } else if ($node instanceof Node\Expr\Array_) {
            $items = [];
            foreach ($node->items as $item) {
                $items[] = new ArrayItem(
                    $this->parseExprNull($item->key),
                    $this->parseExpr($item->value),
                    $item->byRef
                );
            }
            return new Array_($items);
        } else if ($node instanceof Node\Expr\Empty_) {
            return new UnOp(UnOp::EMPTY, $this->parseExpr($node->expr));
        } else if ($node instanceof Node\Expr\ArrayDimFetch) {
            return new ArrayAccess(
                $this->parseExpr($node->var),
                $this->parseExprNull($node->dim)
            );
        } else if ($node instanceof Node\Expr\MethodCall) {
            return new MethodCall(
                $this->parseArgs($node->args),
                $this->parseExpr($node->var),
                $this->parseExprString($node->name)
            );
        } else if ($node instanceof Node\Expr\New_) {
            $class = $node->class;
            if ($class instanceof Node\Stmt\Class_) {
                $class   = new Class_(
                    $this->prefix . 'class' . mt_rand(),
                    $this->parseClassMembers($class)
                );
                $stmts[] = $class;
                $class   = new Literal($class->name());
            } else {
                $class = $this->parseExprClass($class);
            }

            return new New_($class, $this->parseArgs($node->args));
        } else if ($node instanceof Node\Expr\BooleanNot) {
            return new UnOp(UnOp::BOOL_NOT, $this->parseExpr($node->expr));
        } else if ($node instanceof Node\Expr\Print_) {
            return new UnOp(UnOp::PRINT, $this->parseExpr($node->expr));
        } else if ($node instanceof Node\Expr\Closure) {
            $uses = [];
            foreach ($node->uses as $use) {
                $uses[] = new ClosureUse($use->var, $use->byRef);
            }
            return new Closure(
                $node->static,
                $this->parseFunctionType($node),
                $uses,
                $this->parseStmts($node->stmts)
            );
        } else if ($node instanceof Node\Expr\Ternary) {
            return new Ternary(
                $this->parseExpr($node->cond),
                $this->parseExprNull($node->if),
                $this->parseExpr($node->else)
            );
        } else if ($node instanceof Node\Scalar\EncapsedStringPart) {
            return new Literal($node->value);
        } else if ($node instanceof Node\Scalar\Encapsed) {
            $exprs = [];
            foreach ($node->parts as $part) {
                $exprs[] = $this->parseExpr($part);
            }
            return new ConcatMany($exprs);
        } else if ($node instanceof Node\Expr\StaticPropertyFetch) {
            $class = $node->class;
            $prop  = $node->name;

            if ($class instanceof Node\Name) {
                $class = new Literal($this->resolveClass($class));
            } else if ($class instanceof Node\Expr) {
                $class = $this->parseExpr($class);
            } else {
                throw new \Exception('huh?');
            }

            if (is_string($prop)) {
                $prop = new Literal($prop);
            } else if ($prop instanceof Node\Expr) {
                $prop = $this->parseExpr($prop);
            } else {
                throw new \Exception('huh?');
            }

            return new StaticPropertyAccess($class, $prop);
        } else if ($node instanceof Node\Expr\Isset_) {
            return new Isset_($this->parseExprs($node->vars));
        } else if ($node instanceof Node\Expr\BinaryOp) {
            return $this->parseBinaryOp($node);
        } else if ($node instanceof Node\Expr\AssignOp) {
            return $this->parseAssignOp($node);
        } else if ($node instanceof Node\Expr\ErrorSuppress) {
            return new UnOp(UnOp::SUPPRESS, $this->parseExpr($node->expr));
        } else if ($node instanceof Node\Expr\PropertyFetch) {
            return new PropertyAccess(
                $this->parseExpr($node->var),
                $this->parseExprString($node->name)
            );
        } else if ($node instanceof Node\Expr\Exit_) {
            return new Exit_($this->parseExprNull($node->expr));
        } else if ($node instanceof Node\Expr\Eval_) {
            return new UnOp(UnOp::EVAL, $this->parseExpr($node->expr));
        } else if ($node instanceof Node\Expr\Cast) {
            return $this->parseCast($node);
        } else if ($node instanceof Node\Expr\Instanceof_) {
            return new BinOp(
                $this->parseExpr($node->expr),
                BinOp:: INSTANCEOF,
                $this->parseExprClass($node->class)
            );
        } else if ($node instanceof Node\Expr\Clone_) {
            return new UnOp(UnOp::CLONE, $this->parseExpr($node->expr));
        } else if ($node instanceof Node\Expr\Yield_) {
            return new Yield_(
                $this->parseExprNull($node->key),
                $this->parseExprNull($node->value)
            );
        } else if ($node instanceof Node\Expr\ClassConstFetch) {
            return new ClassConstFetch(
                $this->parseExprClass($node->class),
                $node->name
            );
        } else if ($node instanceof Node\Expr\UnaryMinus) {
            return new UnOp(UnOp::NEGATE, $this->parseExpr($node->expr));
        } else if ($node instanceof Node\Expr\UnaryPlus) {
            return new UnOp(UnOp::PLUS, $this->parseExpr($node->expr));
        } else if ($node instanceof Node\Expr\PostInc) {
            return new UnOp(UnOp::POST_INC, $this->parseExpr($node->var));
        } else if ($node instanceof Node\Expr\PreInc) {
            return new UnOp(UnOp::PRE_INC, $this->parseExpr($node->var));
        } else if ($node instanceof Node\Expr\PostDec) {
            return new UnOp(UnOp::POST_DEC, $this->parseExpr($node->var));
        } else if ($node instanceof Node\Expr\PreDec) {
            return new UnOp(UnOp::PRE_DEC, $this->parseExpr($node->var));
        } else if ($node instanceof Node\Expr\List_) {
            $exprs = [];
            foreach ($node->vars as $v) {
                $exprs[] = $this->parseExprNull($v);
            }
            return new List_($exprs);
        } else {
            throw new \Exception('Unhandled expression type: ' . get_class($node));
        }
    }

    private function parseCast(Node\Expr\Cast $node):Expr {
        if ($node instanceof Node\Expr\Cast\Array_) {
            $type = Cast::ARRAY;
        } else if ($node instanceof Node\Expr\Cast\Bool_) {
            $type = Cast::BOOL;
        } else if ($node instanceof Node\Expr\Cast\Double) {
            $type = Cast::FLOAT;
        } else if ($node instanceof Node\Expr\Cast\Int_) {
            $type = Cast::INT;
        } else if ($node instanceof Node\Expr\Cast\Object_) {
            $type = Cast::OBJECT;
        } else if ($node instanceof Node\Expr\Cast\String_) {
            $type = Cast::STRING;
        } else if ($node instanceof Node\Expr\Cast\Unset_) {
            $type = Cast::UNSET;
        } else {
            throw new \Exception('Unknown cast type: ' . get_class($node));
        }

        return new Cast($type, $this->parseExpr($node->expr));
    }

    private function parseAssignOp(Node\Expr\AssignOp $node):Expr {
        if ($node instanceof Node\Expr\AssignOp\BitwiseAnd) {
            $type = BinOp::ASSIGN_BIT_AND;
        } else if ($node instanceof Node\Expr\AssignOp\BitwiseOr) {
            $type = BinOp::ASSIGN_BIT_OR;
        } else if ($node instanceof Node\Expr\AssignOp\BitwiseXor) {
            $type = BinOp::ASSIGN_BIT_XOR;
        } else if ($node instanceof Node\Expr\AssignOp\Concat) {
            $type = BinOp::ASSIGN_CONCAT;
        } else if ($node instanceof Node\Expr\AssignOp\Div) {
            $type = BinOp::ASSIGN_DIVIDE;
        } else if ($node instanceof Node\Expr\AssignOp\Minus) {
            $type = BinOp::ASSIGN_SUBTRACT;
        } else if ($node instanceof Node\Expr\AssignOp\Mod) {
            $type = BinOp::ASSIGN_MODULUS;
        } else if ($node instanceof Node\Expr\AssignOp\Mul) {
            $type = BinOp::ASSIGN_MULTIPLY;
        } else if ($node instanceof Node\Expr\AssignOp\Plus) {
            $type = BinOp::ASSIGN_ADD;
        } else if ($node instanceof Node\Expr\AssignOp\Pow) {
            $type = BinOp::ASSIGN_EXPONENT;
        } else if ($node instanceof Node\Expr\AssignOp\ShiftLeft) {
            $type = BinOp::ASSIGN_SHIFT_LEFT;
        } else if ($node instanceof Node\Expr\AssignOp\ShiftRight) {
            $type = BinOp::ASSIGN_SHIFT_RIGHT;
        } else {
            throw new \Exception('Unhandled assignment operator: ' . get_class($node));
        }

        $left  = $this->parseExpr($node->var);
        $right = $this->parseExpr($node->expr);
        return new BinOp($left, $type, $right);
    }

    private function parseBinaryOp(Node\Expr\BinaryOp $node):Expr {
        if ($node instanceof Node\Expr\BinaryOp\BitwiseAnd) {
            $type = BinOp::BIT_AND;
        } else if ($node instanceof Node\Expr\BinaryOp\BitwiseOr) {
            $type = BinOp::BIT_OR;
        } else if ($node instanceof Node\Expr\BinaryOp\BitwiseXor) {
            $type = BinOp::BIT_XOR;
        } else if ($node instanceof Node\Expr\BinaryOp\BooleanAnd) {
            $type = BinOp::BOOl_AND;
        } else if ($node instanceof Node\Expr\BinaryOp\BooleanOr) {
            $type = BinOp::BOOl_OR;
        } else if ($node instanceof Node\Expr\BinaryOp\Coalesce) {
            $type = BinOp::COALESCE;
        } else if ($node instanceof Node\Expr\BinaryOp\Concat) {
            $type = BinOp::CONCAT;
        } else if ($node instanceof Node\Expr\BinaryOp\Div) {
            $type = BinOp::DIVIDE;
        } else if ($node instanceof Node\Expr\BinaryOp\Equal) {
            $type = BinOp::EQUAL;
        } else if ($node instanceof Node\Expr\BinaryOp\Greater) {
            $type = BinOp::GREATER;
        } else if ($node instanceof Node\Expr\BinaryOp\GreaterOrEqual) {
            $type = BinOp::GREATER_OR_EQUAL;
        } else if ($node instanceof Node\Expr\BinaryOp\Identical) {
            $type = BinOp::IDENTICAL;
        } else if ($node instanceof Node\Expr\BinaryOp\LogicalAnd) {
            $type = BinOp::LOGIC_AND;
        } else if ($node instanceof Node\Expr\BinaryOp\LogicalOr) {
            $type = BinOp::LOGIC_OR;
        } else if ($node instanceof Node\Expr\BinaryOp\LogicalXor) {
            $type = BinOp::LOGIC_XOR;
        } else if ($node instanceof Node\Expr\BinaryOp\Minus) {
            $type = BinOp::SUBTRACT;
        } else if ($node instanceof Node\Expr\BinaryOp\Mod) {
            $type = BinOp::MODULUS;
        } else if ($node instanceof Node\Expr\BinaryOp\Mul) {
            $type = BinOp::MULTIPLY;
        } else if ($node instanceof Node\Expr\BinaryOp\NotEqual) {
            $type = BinOp::NOT_EQUAL;
        } else if ($node instanceof Node\Expr\BinaryOp\NotIdentical) {
            $type = BinOp::NOT_IDENTICAL;
        } else if ($node instanceof Node\Expr\BinaryOp\Plus) {
            $type = BinOp::ADD;
        } else if ($node instanceof Node\Expr\BinaryOp\Pow) {
            $type = BinOp::EXPONENT;
        } else if ($node instanceof Node\Expr\BinaryOp\ShiftLeft) {
            $type = BinOp::SHIFT_LEFT;
        } else if ($node instanceof Node\Expr\BinaryOp\ShiftRight) {
            $type = BinOp::SHIFT_RIGHT;
        } else if ($node instanceof Node\Expr\BinaryOp\Smaller) {
            $type = BinOp::LESS;
        } else if ($node instanceof Node\Expr\BinaryOp\SmallerOrEqual) {
            $type = BinOp::LESS_OR_EQUAL;
        } else if ($node instanceof Node\Expr\BinaryOp\Spaceship) {
            $type = BinOp::SPACESHIP;
        } else {
            throw new \Exception('Unhandled binary operator: ' . get_class($node));
        }

        $left  = $this->parseExpr($node->left);
        $right = $this->parseExpr($node->right);
        return new BinOp($left, $type, $right);
    }

    /**
     * @param string $type
     * @param int    $line
     * @return string
     * @throws \Exception
     */
    private function getMagicConstValue($type, $line) {
        switch ($type) {
            case MagicConst::LINE:
                return $line;
            case MagicConst::FILE:
                return $this->__FILE__;
            case MagicConst::DIR:
                return $this->__DIR__;
            case MagicConst::FUNCTION:
                return $this->__FUNCTION__;
            case MagicConst::CLASS_:
                return $this->__CLASS__;
            case MagicConst::TRAIT:
                return $this->__TRAIT__;
            case MagicConst::METHOD:
                return $this->__METHOD__;
            case MagicConst::NAMESPACE:
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
            $result[] = new CallArg(
                $this->parseExpr($arg->value),
                $arg->byRef,
                $arg->unpack
            );
        }
        return $result;
    }

    /**
     * @param string $file
     */
    private function setFile($file) {
        $this->__FILE__ = realpath($file);
        $this->__DIR__  = dirname($this->__FILE__);
    }
}

abstract class Stmt {
    /**
     * @return SingleStmt[]
     */
    abstract function stmts():array;
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

    public function stmts():array {
        return $this->stmts;
    }
}

abstract class SingleStmt extends Stmt {
    final function stmts():array {
        return [$this];
    }
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
    public function isLValue() {
        return false;
    }
}

class ConstFetch extends Expr {
    /** @var string */
    private $name;

    public function __construct(string $name) {
        $this->name = $name;
    }
}

class ClassConstFetch extends Expr {
    /** @var Expr */
    private $class;
    /** @var string */
    private $const;

    /**
     * @param Expr   $class
     * @param string $const
     */
    public function __construct(Expr $class, $const) {
        $this->class = $class;
        $this->const = $const;
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
    /** @var Expr */
    private $left;
    /** @var Expr */
    private $right;

    public function __construct(Expr $left, Expr $right, bool $byRef = false) {
        $this->left  = $left;
        $this->right = $right;
        $this->byRef = $byRef;
    }
}

class Variable extends Expr {
    /** @var Expr */
    private $name;

    public function __construct(Expr $name) {
        $this->name = $name;
    }

    public function isLValue() {
        return true;
    }
}

class PropertyAccess extends Expr {
    /** @var Expr */
    private $object;
    /** @var Expr */
    private $property;

    /**
     * @param Expr $object
     * @param Expr $property
     */
    public function __construct(Expr $object, Expr $property) {
        $this->object   = $object;
        $this->property = $property;
    }

    public function isLValue() {
        return $this->object->isLValue();
    }
}

class StaticPropertyAccess extends Expr {
    /** @var Expr */
    private $class;
    /** @var Expr */
    private $property;

    /**
     * @param Expr $class
     * @param Expr $property
     */
    public function __construct(Expr $class, Expr $property) {
        $this->class    = $class;
        $this->property = $property;
    }

    public function isLValue() {
        return true;
    }
}

class ArrayAccess extends Expr {
    /** @var Expr */
    private $array;
    /** @var Expr|null */
    private $key;

    /**
     * @param Expr      $array
     * @param Expr|null $key
     */
    public function __construct(Expr $array, Expr $key = null) {
        $this->array = $array;
        $this->key   = $key;
    }

    public function isLValue() {
        return $this->array->isLValue();
    }
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
    const LINE      = '__LINE__';
    const FILE      = '__FILE__';
    const DIR       = '__DIR__';
    const FUNCTION  = '__FUNCTION__';
    const CLASS_    = '__CLASS__';
    const TRAIT     = '__TRAIT__';
    const METHOD    = '__METHOD__';
    const NAMESPACE = '__NAMESPACE__';

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

class Visibility {
    const PUBLIC    = 'public';
    const PROTECTED = 'protected';
    const PRIVATE   = 'private';
}

abstract class Classish extends SingleStmt {
    private $name;

    /**
     * @param string $name
     */
    function __construct($name) {
        $this->name = $name;
    }

    function name() {
        return $this->name;
    }

    abstract function members();
}

class Trait_ extends Classish {
    /** @var ClassMember[] */
    private $members = [];

    /**
     * @param string        $name
     * @param ClassMember[] $members
     */
    public function __construct($name, array $members) {
        parent::__construct($name);
        $this->members = $members;
    }

    function members() {
        return $this->members;
    }
}

class Not_ extends Expr {
    private $expr;

    function __construct(Expr $expr) {
        $this->expr = $expr;
    }
}

class Class_ extends Classish {
    /** @var ClassMember */
    private $members = [];

    public function __construct($name, array $members) {
        parent::__construct($name);
        $this->members = $members;
    }

    public function makeAnonymous():Expr {
        return new If_(
            new Not_(new FunctionCall(new Literal('class_exists'), [
                new CallArg(new Literal($this->name()), false, false),
                new CallArg(new Literal(false), false, false),
            ])),
            new StmtBlock([$this]),
            new StmtBlock()
        );
    }

    function members() {
        return $this->members;
    }
}

class Interface_ extends Classish {
    /** @var Method_[] */
    private $methods = [];

    /**
     * @param string    $name
     * @param Method_[] $methods
     */
    public function __construct($name, array $methods) {
        parent::__construct($name);
        $this->methods = $methods;
    }

    public function members() {
        return $this->methods;
    }
}

class Function_ extends SingleStmt {
    /** @var string */
    private $name;
    /** @var Stmt|null */
    private $body;
    /** @var FunctionSignature */
    private $type;

    /**
     * @param string            $name
     * @param FunctionSignature $type
     * @param Stmt|null         $body
     */
    public function __construct($name, FunctionSignature $type, Stmt $body = null) {
        $this->name = $name;
        $this->type = $type;
        $this->body = $body;
    }
}

class Type {
}

abstract class ClassMember {
    /** @var string */
    private $visibility;
    /** @var bool */
    private $static;

    /**
     * @param string $visibility
     * @param bool   $static
     */
    public function __construct($visibility, $static) {
        $this->visibility = $visibility;
        $this->static     = $static;
    }

    public function visibility() {
        return $this->visibility;
    }
}

class Property extends ClassMember {
    /** @var string */
    private $name;
    /** @var Type */
    private $type;
    /** @var Expr|null */
    private $default = null;

    /**
     * @param string    $name
     * @param Type      $type
     * @param Expr|null $default
     * @param string    $visibility
     * @param bool      $static
     */
    public function __construct($name, Type $type, Expr $default = null, $visibility, $static) {
        parent::__construct($visibility, $static);
        $this->name    = $name;
        $this->type    = $type;
        $this->default = $default;
    }
}

class FunctionSignature {
    /** @var bool */
    private $returnRef;
    /** @var Type */
    private $returnType;
    /** @var FunctionParam[] */
    private $params = [];

    /**
     * @param bool            $returnRef
     * @param FunctionParam[] $params
     */
    public function __construct($returnRef, array $params) {
        $this->returnRef = $returnRef;
        $this->params    = $params;
    }
}

class Method_ extends ClassMember {
    /** @var string */
    private $name;
    /** @var FunctionSignature */
    private $type;
    /** @var Stmt|null */
    private $body;
    /** @var bool */
    private $final;

    /**
     * @param string            $name
     * @param FunctionSignature $type
     * @param Stmt|null         $body
     * @param bool              $final
     * @param string            $visibility
     * @param bool              $static
     */
    public function __construct($name, FunctionSignature $type, Stmt $body = null, $final, $visibility, $static) {
        parent::__construct($visibility, $static);
        $this->final = $final;
        $this->name  = $name;
        $this->type  = $type;
        $this->body  = $body;
    }

    public function isAbstract() {
        return $this->body ? false : true;
    }

    public function isFinal() {
        return $this->final ? true : false;
    }
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

class Empty_ extends Expr {
    /** @var Expr */
    private $expr;

    public function __construct(Expr $expr) {
        $this->expr = $expr;
    }
}

class New_ extends Expr {
    /** @var Expr */
    private $class;
    /** @var CallArg[] */
    private $args;

    /**
     * @param Expr      $class
     * @param CallArg[] $args
     */
    public function __construct(Expr $class, array $args) {
        $this->class = $class;
        $this->args  = $args;
    }
}

class Print_ extends Expr {
    /** @var Expr */
    private $expr;

    public function __construct(Expr $expr) {
        $this->expr = $expr;
    }
}

class Closure extends Expr {
    /** @var bool */
    private $static;
    /** @var FunctionSignature */
    private $type;
    /** @var ClosureUse[] */
    private $uses;
    /** @var Stmt */
    private $body;

    /**
     * @param bool              $static
     * @param FunctionSignature $type
     * @param ClosureUse[]      $uses
     * @param Stmt              $body
     */
    public function __construct($static, FunctionSignature $type, array $uses, Stmt $body) {
        $this->static = $static;
        $this->type   = $type;
        $this->uses   = $uses;
        $this->body   = $body;
    }
}

class ClosureUse {
    /** @var string */
    private $name;
    /** @var bool */
    private $byRef;

    /**
     * @param string $name
     * @param bool   $byRef
     */
    public function __construct($name, $byRef) {
        $this->name  = $name;
        $this->byRef = $byRef;
    }
}

class Ternary extends Expr {
    /** @var Expr */
    private $cond;
    /** @var Expr|null */
    private $true;
    /** @var Expr */
    private $false;

    /**
     * @param Expr      $cond
     * @param Expr|null $true
     * @param Expr      $false
     */
    public function __construct(Expr $cond, Expr $true = null, Expr $false) {
        $this->cond  = $cond;
        $this->true  = $true;
        $this->false = $false;
    }
}

class ConcatMany extends Expr {
    /** @var Expr[] */
    private $exprs;

    /**
     * @param Expr[] $exprs
     */
    public function __construct(array $exprs) {
        $this->exprs = $exprs;
    }
}

class Isset_ extends Expr {
    /** @var Expr[] */
    private $exprs;

    /**
     * @param Expr[] $exprs
     */
    public function __construct(array $exprs) {
        $this->exprs = $exprs;
    }
}

class Foreach_ extends SingleStmt {
    /** @var Expr */
    private $array;
    /** @var Expr|null */
    private $key;
    /** @var Expr */
    private $value;
    /** @var Stmt */
    private $body;
    /** @var bool */
    private $byRef;

    /**
     * @param Expr      $array
     * @param Expr|null $key
     * @param Expr      $value
     * @param bool      $byRef
     * @param Stmt      $body
     */
    public function __construct(Expr $array, Expr $key = null, Expr $value, $byRef, Stmt $body) {
        $this->array = $array;
        $this->key   = $key;
        $this->value = $value;
        $this->body  = $body;
        $this->byRef = $byRef;
    }
}

class Echo_ extends SingleStmt {
    /** @var Expr[] */
    private $exprs;

    /**
     * @param Expr[] $exprs
     */
    public function __construct(array $exprs) {
        $this->exprs = $exprs;
    }
}

class BinOp extends Expr {
    // Arithmetic
    const ADD      = '+';
    const SUBTRACT = '-';
    const MULTIPLY = '*';
    const DIVIDE   = '/';
    const MODULUS  = '%';
    const EXPONENT = '**';

    // Bitwise
    const BIT_AND     = '&';
    const BIT_OR      = '|';
    const BIT_XOR     = '^';
    const SHIFT_LEFT  = '<<';
    const SHIFT_RIGHT = '>>';

    // Comparison
    const EQUAL            = '==';
    const IDENTICAL        = '===';
    const NOT_EQUAL        = '!=';
    const NOT_IDENTICAL    = '!==';
    const GREATER          = '>';
    const LESS             = '<';
    const GREATER_OR_EQUAL = '>=';
    const LESS_OR_EQUAL    = '<=';
    const SPACESHIP        = '<=>';
    const COALESCE         = '??';

    // Logical
    const BOOl_AND  = '&&';
    const BOOl_OR   = '||';
    const LOGIC_AND = 'and';
    const LOGIC_OR  = 'or';
    const LOGIC_XOR = 'xor';

    // String
    const CONCAT     = '.';

    // Type
    const INSTANCEOF = 'instanceof';

    // Assignment
    const ASSIGN             = '=';
    const ASSIGN_REF         = '=&';
    const ASSIGN_ADD         = '+=';
    const ASSIGN_SUBTRACT    = '-=';
    const ASSIGN_MULTIPLY    = '*=';
    const ASSIGN_DIVIDE      = '/=';
    const ASSIGN_MODULUS     = '%=';
    const ASSIGN_EXPONENT    = '**=';
    const ASSIGN_CONCAT      = '.=';
    const ASSIGN_BIT_AND     = '&=';
    const ASSIGN_BIT_OR      = '|=';
    const ASSIGN_BIT_XOR     = '^=';
    const ASSIGN_SHIFT_LEFT  = '<<=';
    const ASSIGN_SHIFT_RIGHT = '>>=';

    /** @var string */
    private $type;
    /** @var Expr */
    private $left;
    /** @var Expr */
    private $right;

    /**
     * @param Expr   $left
     * @param string $type
     * @param Expr   $right
     */
    public function __construct(Expr $left, $type, Expr $right) {
        $this->left  = $left;
        $this->type  = $type;
        $this->right = $right;
    }
}

class Cast extends Expr {
    const INT    = 'int';
    const BOOL   = 'bool';
    const FLOAT  = 'float';
    const STRING = 'string';
    const ARRAY  = 'array';
    const OBJECT = 'object';
    const UNSET  = 'unset';

    /** @var string */
    private $type;
    /** @var Expr */
    private $expr;

    /**
     * @param string $type
     * @param Expr   $expr
     */
    public function __construct($type, Expr $expr) {
        $this->type = $type;
        $this->expr = $expr;
    }
}

class UnOp extends Expr {
    const PRE_INC  = '++ ';
    const POST_INC = ' ++';
    const PRE_DEC  = '-- ';
    const POST_DEC = ' --';
    const PRINT    = 'print';
    const BOOL_NOT = '!';
    const BIT_NOT  = '~';
    const NEGATE   = '-';
    const PLUS     = '+';
    const SUPPRESS = '@';
    const EMPTY    = 'empty';
    const EVAL     = 'eval';
    const CLONE    = 'clone';

    /** @var string */
    private $type;
    /** @var Expr */
    private $expr;

    /**
     * @param string $type
     * @param Expr   $expr
     */
    public function __construct($type, Expr $expr) {
        $this->type = $type;
        $this->expr = $expr;
    }
}

class Exec extends Expr {

}

class InlineHTML extends Expr {
    /** @var string */
    private $html;

    /**
     * @param string $html
     */
    public function __construct($html) {
        $this->html = $html;
    }
}

class Exit_ extends Expr {
    /** @var Expr|null */
    private $expr;

    /**
     * @param Expr|null $expr
     */
    public function __construct(Expr $expr = null) {
        $this->expr = $expr;
    }
}

class Const_ extends SingleStmt {
    /** @var string */
    private $name;
    /** @var Expr */
    private $value;

    /**
     * @param string $name
     * @param Expr   $value
     */
    public function __construct($name, Expr $value) {
        $this->name  = $name;
        $this->value = $value;
    }
}

class Throw_ extends SingleStmt {
    /** @var Expr */
    private $expr;

    /**
     * @param Expr $expr
     */
    public function __construct(Expr $expr) {
        $this->expr = $expr;
    }
}

class StaticVar extends SingleStmt {
    /** @var string */
    private $name;
    /** @var Expr|null */
    private $value;

    /**
     * @param string    $name
     * @param Expr|null $value
     */
    public function __construct($name, Expr $value = null) {
        $this->name  = $name;
        $this->value = $value;
    }
}

class For_ extends SingleStmt {
    /** @var Expr[] */
    private $init = [];
    /** @var Expr[] */
    private $cond = [];
    /** @var Expr[] */
    private $loop = [];
    /** @var Stmt */
    private $body;

    /**
     * @param Expr[] $init
     * @param Expr[] $cond
     * @param Expr[] $loop
     * @param Stmt   $body
     */
    public function __construct(array $init, array $cond, array $loop, Stmt $body) {
        $this->init = $init;
        $this->cond = $cond;
        $this->loop = $loop;
        $this->body = $body;
    }
}

class Break_ extends SingleStmt {
    /** @var int */
    private $levels;

    /**
     * @param int $levels
     */
    public function __construct($levels = 1) {
        $this->levels = $levels;
    }
}

class Continue_ extends SingleStmt {
    /** @var int */
    private $levels;

    /**
     * @param int $levels
     */
    public function __construct($levels) {
        $this->levels = $levels;
    }
}

class Yield_ extends Expr {
    /** @var Expr|null */
    private $key;
    /** @var Expr|null */
    private $value;

    /**
     * @param Expr|null $key
     * @param Expr|null $value
     */
    public function __construct(Expr $key = null, Expr $value = null) {
        $this->key   = $key;
        $this->value = $value;
    }

}

class Switch_ extends SingleStmt {
    /** @var Expr */
    private $expr;
    /** @var Case_[] */
    private $cases;

    /**
     * @param Expr    $expr
     * @param Case_[] $cases
     */
    public function __construct(Expr $expr, array $cases) {
        $this->expr  = $expr;
        $this->cases = $cases;
    }
}

class Case_ {
    /** @var Expr|null */
    private $expr;
    /** @var Stmt */
    private $stmt;

    /**
     * @param Expr|null $expr
     * @param Stmt      $stmt
     */
    public function __construct(Expr $expr = null, Stmt $stmt) {
        $this->expr = $expr;
        $this->stmt = $stmt;
    }
}

class Unset_ extends SingleStmt {
    /** @var Expr[] */
    private $exprs;

    /**
     * @param Expr[] $exprs
     */
    public function __construct(array $exprs) {
        $this->exprs = $exprs;
    }
}

class While_ extends SingleStmt {
    /** @var Expr */
    private $cond;
    /** @var Stmt */
    private $body;

    /**
     * @param Expr $cond
     * @param Stmt $body
     */
    public function __construct(Expr $cond, Stmt $body) {
        $this->cond = $cond;
        $this->body = $body;
    }
}

class List_ extends Expr {
    /** @var (Expr|null)[] */
    private $exprs;

    /**
     * @param (Expr|null)[] $exprs
     */
    public function __construct(array $exprs) {
        $this->exprs = $exprs;
    }
}