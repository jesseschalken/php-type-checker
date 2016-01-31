<?php

namespace JesseSchalken\PhpTypeChecker\Expr;

use JesseSchalken\PhpTypeChecker\Type;
use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Stmt;

abstract class Expr extends Stmt\SingleStmt {
    public function isLValue():bool {
        return false;
    }

    public final function unparseStmt() {
        return $this->unparseExpr();
    }

    public abstract function unparseExpr():\PhpParser\Node\Expr;

    /**
     * @return \PhpParser\Node\Expr|\PhpParser\Node\Name
     */
    public function unparseExprOrName() {
        return $this->unparseExpr();
    }

    /**
     * @return \PhpParser\Node\Expr|string
     */
    public function unparseExprOrString() {
        return $this->unparseExpr();
    }
}

class Yield_ extends Expr {
    /** @var Expr|null */
    private $key;
    /** @var Expr|null */
    private $val;

    public function __construct(CodeLoc $loc, Expr $key = null, Expr $val = null) {
        parent::__construct($loc);
        $this->key = $key;
        $this->val = $val;
    }

    public function subStmts():array {
        $stmts = [];
        if ($this->key) {
            $stmts[] = $this->key;
        }
        if ($this->val) {
            $stmts[] = $this->val;
        }
        return $stmts;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\Yield_(
            $this->val ? $this->val->unparseExpr() : null,
            $this->key ? $this->key->unparseExpr() : null
        );
    }
}

class Include_ extends Expr {
    /** @var bool */
    private $require = true;
    /** @var bool */
    private $once = true;
    /** @var Expr */
    private $expr;

    public function __construct(CodeLoc $loc, Expr $expr, bool $require, bool $once) {
        parent::__construct($loc);
        $this->require = $require;
        $this->once    = $once;
        $this->expr    = $expr;
    }

    public function subStmts():array {
        return [$this->expr];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $type = $this->require
            ? ($this->once
                ? \PhpParser\Node\Expr\Include_::TYPE_REQUIRE_ONCE
                : \PhpParser\Node\Expr\Include_::TYPE_REQUIRE)
            : ($this->once
                ? \PhpParser\Node\Expr\Include_::TYPE_INCLUDE_ONCE
                : \PhpParser\Node\Expr\Include_::TYPE_INCLUDE);

        return new \PhpParser\Node\Expr\Include_($this->expr->unparseExpr(), $type);
    }
}

class Array_ extends Expr {
    /** @var ArrayItem[] */
    private $items = [];

    /**
     * @param CodeLoc     $loc
     * @param ArrayItem[] $items
     */
    public function __construct(CodeLoc $loc, array $items) {
        parent::__construct($loc);
        $this->items = $items;
    }

    public function subStmts():array {
        $stmts = [];
        foreach ($this->items as $item) {
            foreach ($item->subStmts() as $stmt) {
                $stmts[] = $stmt;
            }
        }
        return $stmts;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $items = [];
        foreach ($this->items as $item) {
            $items[] = $item->unparse();
        }
        return new \PhpParser\Node\Expr\Array_($items);
    }
}

class ArrayItem extends Node {
    /** @var Expr|null */
    private $key;
    /** @var Expr */
    private $value;
    /** @var bool */
    private $byRef;

    /**
     * @param CodeLoc   $loc
     * @param Expr|null $key
     * @param Expr      $value
     * @param bool      $byRef
     */
    public function __construct(CodeLoc $loc, Expr $key = null, Expr $value, bool $byRef) {
        parent::__construct($loc);
        $this->key   = $key;
        $this->value = $value;
        $this->byRef = $byRef;
    }

    public function subStmts():array {
        $stmts = [$this->value];
        if ($this->key) {
            $stmts[] = $this->key;
        }
        return $stmts;
    }

    public function unparse():\PhpParser\Node\Expr\ArrayItem {
        return new \PhpParser\Node\Expr\ArrayItem(
            $this->value->unparseExpr(),
            $this->key ? $this->key->unparseExpr() : null,
            $this->byRef
        );
    }
}

class Print_ extends Expr {
    /** @var Expr */
    private $expr;

    public function __construct(CodeLoc $loc, Expr $expr) {
        parent::__construct($loc);
        $this->expr = $expr;
    }

    public function subStmts():array {
        return [$this->expr];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\Print_($this->expr->unparseExpr());
    }
}

class Closure extends Expr {
    /** @var bool */
    private $static;
    /** @var Stmt\FunctionSignature */
    private $type;
    /** @var ClosureUse[] */
    private $uses;
    /** @var Stmt\Block */
    private $body;

    /**
     * @param CodeLoc                $loc
     * @param bool                   $static
     * @param Stmt\FunctionSignature $type
     * @param ClosureUse[]           $uses
     * @param Stmt\Block             $body
     */
    public function __construct(
        CodeLoc $loc,
        bool $static,
        Stmt\FunctionSignature $type,
        array $uses,
        Stmt\Block $body
    ) {
        parent::__construct($loc);
        $this->static = $static;
        $this->type   = $type;
        $this->uses   = $uses;
        $this->body   = $body;
    }

    public function subStmts():array {
        $stmts = [$this->body];
        $stmts = array_merge($stmts, $this->type->subStmts());
        return $stmts;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $subNodes = $this->type->unparseAttributes();

        $uses = [];
        foreach ($this->uses as $use) {
            $uses[] = $use->unparse();
        }

        return new \PhpParser\Node\Expr\Closure(array_replace($subNodes, [
            'static' => $this->static,
            'uses'   => $uses,
            'stmts'  => $this->body->unparseNodes(),
        ]));
    }
}

class ClosureUse extends Node {
    /** @var string */
    private $name;
    /** @var bool */
    private $byRef;

    public function __construct(CodeLoc $loc, string $name, bool $byRef) {
        parent::__construct($loc);
        $this->name  = $name;
        $this->byRef = $byRef;
    }

    public function unparse():\PhpParser\Node\Expr\ClosureUse {
        return new \PhpParser\Node\Expr\ClosureUse($this->name, $this->byRef);
    }
}

class Ternary extends Expr {
    /** @var Expr */
    private $cond;
    /** @var Expr|null */
    private $true;
    /** @var Expr */
    private $false;

    public function __construct(CodeLoc $loc, Expr $cond, Expr $true = null, Expr $false) {
        parent::__construct($loc);
        $this->cond  = $cond;
        $this->true  = $true;
        $this->false = $false;
    }

    public function subStmts():array {
        $stmts = [$this->cond, $this->false];
        if ($this->true) {
            $stmts[] = $this->true;
        }
        return $stmts;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\Ternary(
            $this->cond->unparseExpr(),
            $this->true ? $this->true->unparseExpr() : null,
            $this->false->unparseExpr()
        );
    }
}

class ConcatMany extends Expr {
    /**
     * @param Expr[] $exprs
     * @return \PhpParser\Node\Expr[]
     */
    public static function unparseEncaps(array $exprs):array {
        $parts = [];
        foreach ($exprs as $expr) {
            $expr = $expr->unparseExprOrString();
            if (is_string($expr)) {
                $expr = new \PhpParser\Node\Scalar\EncapsedStringPart($expr);
            }
            $parts[] = $expr;
        }
        return $parts;
    }

    /** @var Expr[] */
    private $exprs;

    /**
     * @param CodeLoc $loc
     * @param Expr[]  $exprs
     */
    public function __construct(CodeLoc $loc, array $exprs) {
        parent::__construct($loc);
        $this->exprs = $exprs;
    }

    public function subStmts():array {
        return $this->exprs;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Scalar\Encapsed(self::unparseEncaps($this->exprs));
    }
}

class Isset_ extends Expr {
    /** @var Expr[] */
    private $exprs;

    /**
     * @param CodeLoc $loc
     * @param Expr[]  $exprs
     */
    public function __construct(CodeLoc $loc, array $exprs) {
        parent::__construct($loc);
        $this->exprs = $exprs;
    }

    public function subStmts():array {
        return $this->exprs;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $exprs = [];
        foreach ($this->exprs as $expr) {
            $exprs[] = $expr->unparseExpr();
        }
        return new \PhpParser\Node\Expr\Isset_($exprs);
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

    public function __construct(CodeLoc $loc, Expr $left, string $type, Expr $right) {
        parent::__construct($loc);
        $this->left  = $left;
        $this->type  = $type;
        $this->right = $right;
    }

    public function subStmts():array {
        return [$this->left, $this->right];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        switch ($this->type) {
            case self:: INSTANCEOF:
                return new \PhpParser\Node\Expr\Instanceof_(
                    $this->left->unparseExpr(),
                    $this->right->unparseExprOrName()
                );
        }
        $left  = $this->left->unparseExpr();
        $right = $this->right->unparseExpr();
        switch ($this->type) {
            case self::ADD:
                return new \PhpParser\Node\Expr\BinaryOp\Plus($left, $right);
            case self::SUBTRACT:
                return new \PhpParser\Node\Expr\BinaryOp\Minus($left, $right);
            case self::MULTIPLY:
                return new \PhpParser\Node\Expr\BinaryOp\Mul($left, $right);
            case self::DIVIDE:
                return new \PhpParser\Node\Expr\BinaryOp\Div($left, $right);
            case self::MODULUS:
                return new \PhpParser\Node\Expr\BinaryOp\Mod($left, $right);
            case self::EXPONENT:
                return new \PhpParser\Node\Expr\BinaryOp\Pow($left, $right);
            case self::BIT_AND:
                return new \PhpParser\Node\Expr\BinaryOp\BitwiseAnd($left, $right);
            case self::BIT_OR:
                return new \PhpParser\Node\Expr\BinaryOp\BitwiseOr($left, $right);
            case self::BIT_XOR:
                return new \PhpParser\Node\Expr\BinaryOp\BitwiseXor($left, $right);
            case self::SHIFT_LEFT:
                return new \PhpParser\Node\Expr\BinaryOp\ShiftLeft($left, $right);
            case self::SHIFT_RIGHT:
                return new \PhpParser\Node\Expr\BinaryOp\ShiftRight($left, $right);
            case self::EQUAL:
                return new \PhpParser\Node\Expr\BinaryOp\Equal($left, $right);
            case self::IDENTICAL:
                return new \PhpParser\Node\Expr\BinaryOp\Identical($left, $right);
            case self::NOT_EQUAL:
                return new \PhpParser\Node\Expr\BinaryOp\NotEqual($left, $right);
            case self::NOT_IDENTICAL:
                return new \PhpParser\Node\Expr\BinaryOp\NotIdentical($left, $right);
            case self::GREATER:
                return new \PhpParser\Node\Expr\BinaryOp\Greater($left, $right);
            case self::LESS:
                return new \PhpParser\Node\Expr\BinaryOp\Smaller($left, $right);
            case self::GREATER_OR_EQUAL:
                return new \PhpParser\Node\Expr\BinaryOp\GreaterOrEqual($left, $right);
            case self::LESS_OR_EQUAL:
                return new \PhpParser\Node\Expr\BinaryOp\SmallerOrEqual($left, $right);
            case self::SPACESHIP:
                return new \PhpParser\Node\Expr\BinaryOp\Spaceship($left, $right);
            case self::COALESCE:
                return new \PhpParser\Node\Expr\BinaryOp\Coalesce($left, $right);
            case self::BOOl_AND:
                return new \PhpParser\Node\Expr\BinaryOp\BooleanAnd($left, $right);
            case self::BOOl_OR:
                return new \PhpParser\Node\Expr\BinaryOp\BooleanOr($left, $right);
            case self::LOGIC_AND:
                return new \PhpParser\Node\Expr\BinaryOp\LogicalAnd($left, $right);
            case self::LOGIC_OR:
                return new \PhpParser\Node\Expr\BinaryOp\LogicalOr($left, $right);
            case self::LOGIC_XOR:
                return new \PhpParser\Node\Expr\BinaryOp\LogicalXor($left, $right);
            case self::CONCAT:
                return new \PhpParser\Node\Expr\BinaryOp\Concat($left, $right);
            case self::ASSIGN:
                return new \PhpParser\Node\Expr\Assign($left, $right);
            case self::ASSIGN_REF:
                return new \PhpParser\Node\Expr\AssignRef($left, $right);
            case self::ASSIGN_ADD:
                return new \PhpParser\Node\Expr\AssignOp\Plus($left, $right);
            case self::ASSIGN_SUBTRACT:
                return new \PhpParser\Node\Expr\AssignOp\Minus($left, $right);
            case self::ASSIGN_MULTIPLY:
                return new \PhpParser\Node\Expr\AssignOp\Mul($left, $right);
            case self::ASSIGN_DIVIDE:
                return new \PhpParser\Node\Expr\AssignOp\Div($left, $right);
            case self::ASSIGN_MODULUS:
                return new \PhpParser\Node\Expr\AssignOp\Mod($left, $right);
            case self::ASSIGN_EXPONENT:
                return new \PhpParser\Node\Expr\AssignOp\Pow($left, $right);
            case self::ASSIGN_CONCAT:
                return new \PhpParser\Node\Expr\AssignOp\Concat($left, $right);
            case self::ASSIGN_BIT_AND:
                return new \PhpParser\Node\Expr\AssignOp\BitwiseAnd($left, $right);
            case self::ASSIGN_BIT_OR:
                return new \PhpParser\Node\Expr\AssignOp\BitwiseOr($left, $right);
            case self::ASSIGN_BIT_XOR:
                return new \PhpParser\Node\Expr\AssignOp\BitwiseXor($left, $right);
            case self::ASSIGN_SHIFT_LEFT:
                return new \PhpParser\Node\Expr\AssignOp\ShiftLeft($left, $right);
            case self::ASSIGN_SHIFT_RIGHT:
                return new \PhpParser\Node\Expr\AssignOp\ShiftRight($left, $right);

            default:
                throw new \Exception('Invalid binary operator type: ' . $this->type);
        }
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

    public function __construct(CodeLoc $loc, string $type, Expr $expr) {
        parent::__construct($loc);
        $this->type = $type;
        $this->expr = $expr;
    }

    public function subStmts():array {
        return [$this->expr];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $expr = $this->expr->unparseExpr();
        switch ($this->type) {
            case self::INT:
                return new \PhpParser\Node\Expr\Cast\Int_($expr);
            case self::BOOL:
                return new \PhpParser\Node\Expr\Cast\Bool_($expr);
            case self::FLOAT:
                return new \PhpParser\Node\Expr\Cast\Double($expr);
            case self::STRING:
                return new \PhpParser\Node\Expr\Cast\String_($expr);
            case self::ARRAY:
                return new \PhpParser\Node\Expr\Cast\Array_($expr);
            case self::OBJECT:
                return new \PhpParser\Node\Expr\Cast\Object_($expr);
            case self::UNSET:
                return new \PhpParser\Node\Expr\Cast\Unset_($expr);
            default:
                throw new \Exception('Invalid cast type: ' . $this->type);
        }
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

    public function __construct(CodeLoc $loc, string $type, Expr $expr) {
        parent::__construct($loc);
        $this->type = $type;
        $this->expr = $expr;
    }

    public function subStmts():array {
        return [$this->expr];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $expr = $this->expr->unparseExpr();
        switch ($this->type) {
            case self::PRE_INC:
                return new \PhpParser\Node\Expr\PreInc($expr);
            case self::PRE_DEC:
                return new \PhpParser\Node\Expr\PreDec($expr);
            case self::POST_INC:
                return new \PhpParser\Node\Expr\PostInc($expr);
            case self::POST_DEC:
                return new \PhpParser\Node\Expr\PostDec($expr);
            case self::PRINT:
                return new \PhpParser\Node\Expr\Print_($expr);
            case self::BOOL_NOT:
                return new \PhpParser\Node\Expr\BooleanNot($expr);
            case self::BIT_NOT:
                return new \PhpParser\Node\Expr\BitwiseNot($expr);
            case self::PLUS:
                return new \PhpParser\Node\Expr\UnaryPlus($expr);
            case self::NEGATE:
                return new \PhpParser\Node\Expr\UnaryMinus($expr);
            case self::SUPPRESS:
                return new \PhpParser\Node\Expr\ErrorSuppress($expr);
            case self::EMPTY:
                return new \PhpParser\Node\Expr\Empty_($expr);
            case self::EVAL:
                return new \PhpParser\Node\Expr\Eval_($expr);
            case self::CLONE:
                return new \PhpParser\Node\Expr\Clone_($expr);
            default:
                throw new \Exception('Invalid unary operator type: ' . $this->type);
        }
    }
}

class Exit_ extends Expr {
    /** @var Expr|null */
    private $expr;

    public function __construct(CodeLoc $loc, Expr $expr = null) {
        parent::__construct($loc);
        $this->expr = $expr;
    }

    public function subStmts():array {
        return $this->expr ? [$this->expr] : [];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $expr = $this->expr ? $this->expr->unparseExpr() : null;
        return new \PhpParser\Node\Expr\Exit_($expr);
    }
}

class ShellExec extends Expr {
    /** @var Expr[] */
    private $parts;

    /**
     * @param CodeLoc $loc
     * @param Expr[]  $parts
     */
    public function __construct(CodeLoc $loc, array $parts) {
        parent::__construct($loc);
        $this->parts = $parts;
    }

    public function subStmts():array {
        return $this->parts;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\ShellExec(ConcatMany::unparseEncaps($this->parts));
    }
}

abstract class AbstractClassName extends Expr {
    public abstract function toString(string $static = null):string;

    public abstract function toType(string $static = null, bool $strict = false):Type\Type;
}

/**
 * Foo\Bar::class
 */
class ClassName extends AbstractClassName {
    /** @var string */
    private $class;

    public function __construct(CodeLoc $loc, string $class) {
        parent::__construct($loc);
        if (substr($class, 0, 1) === '\\') {
            throw new \Exception("Illegal class name: $class");
        }
        $this->class = $class;
    }

    public function toType(string $static = null, bool $strict = false):Type\Type {
        return new Type\Class_($this->loc(), $this->class);
    }

    public function toString(string $static = null):string {
        return $this->class;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\ClassConstFetch(
            new \PhpParser\Node\Name\FullyQualified($this->class),
            'class'
        );
    }

    public function unparseExprOrName() {
        return new \PhpParser\Node\Name\FullyQualified($this->class);
    }

    public function unparseExprOrString() {
        return $this->class;
    }
}

/**
 * static::name
 */
class StaticClassName extends AbstractClassName {
    public function toString(string $static = null):string {
        if ($static === null) {
            throw new \Exception('"static" used in disallowed context');
        } else {
            return $static;
        }
    }

    public function toType(string $static = null, bool $strict = false):Type\Type {
        $type = new Type\Class_($this->loc(), $this->toString($static));
        $type = new Type\TypeVar($this->loc(), Type\TypeVar::STATIC, $type);
        return $type;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\ClassConstFetch(
            new \PhpParser\Node\Name('static'),
            'class'
        );
    }

    public function unparseExprOrName() {
        return new \PhpParser\Node\Name('static');
    }
}
