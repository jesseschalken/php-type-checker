<?php

namespace JesseSchalken\PhpTypeChecker\Expr;

use JesseSchalken\PhpTypeChecker\Context;
use JesseSchalken\PhpTypeChecker\Function_;
use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Stmt;
use JesseSchalken\PhpTypeChecker\Type;

abstract class Expr extends Stmt\SingleStmt {
    public function isLValue():bool {
        return false;
    }

    public function isCall():bool {
        return false;
    }

    public final function isReferrable():bool {
        return $this->isLValue() || $this->isCall();
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

    public final function checkStmt(Context\Context $context) {
        $this->checkExpr($context, false);
    }

    /**
     * @param Context\Context $context
     * @param bool            $noErrors If true, don't report any errors. Just do the minimal amount of work required
     *                                  to get the return type of this expression.
     * @return Type\Type
     */
    public abstract function checkExpr(Context\Context $context, bool $noErrors):Type\Type;

    /**
     * @param Type\Type       $type
     * @param Context\Context $context
     * @return array|Type\Type[]
     */
    public function inferLocal(Type\Type $type, Context\Context $context):array {
        return [];
    }
}

class Yield_ extends Expr {
    /** @var Expr|null */
    private $key;
    /** @var Expr|null */
    private $val;

    public function __construct(HasCodeLoc $loc, Expr $key = null, Expr $val = null) {
        parent::__construct($loc);
        $this->key = $key;
        $this->val = $val;
    }

    public function subStmts(bool $deep):array {
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

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        return new Type\Mixed($this);
    }
}

class Include_ extends Expr {
    /** @var bool */
    private $require = true;
    /** @var bool */
    private $once = true;
    /** @var Expr */
    private $expr;

    public function __construct(HasCodeLoc $loc, Expr $expr, bool $require, bool $once) {
        parent::__construct($loc);
        $this->require = $require;
        $this->once    = $once;
        $this->expr    = $expr;
    }

    public function subStmts(bool $deep):array {
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

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        return new Type\Mixed($this);
    }
}

class Array_ extends Expr {
    /** @var ArrayItem[] */
    private $items = [];

    /**
     * @param HasCodeLoc  $loc
     * @param ArrayItem[] $items
     */
    public function __construct(HasCodeLoc $loc, array $items) {
        parent::__construct($loc);
        $this->items = $items;
    }

    public function subStmts(bool $deep):array {
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

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        $array = new Type\Shape($this, []);
        foreach ($this->items as $item) {
            $array = $item->apply($array, $context, $noErrors);
        }
        return $array;
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
     * @param HasCodeLoc $loc
     * @param Expr|null  $key
     * @param Expr       $value
     * @param bool       $byByRef
     */
    public function __construct(HasCodeLoc $loc, Expr $key = null, Expr $value, bool $byByRef) {
        parent::__construct($loc);
        $this->key   = $key;
        $this->value = $value;
        $this->byRef = $byByRef;
    }

    public function subStmts():array {
        $stmts = [$this->value];
        if ($this->key) {
            $stmts[] = $this->key;
        }
        return $stmts;
    }

    public function apply(Type\Type $array, Context\Context $context, bool $noErrors) {
        $key = $this->key;
        $val = $this->value->checkExpr($context, $noErrors);
        if ($key) {
            return $key->checkExpr($context, $noErrors)->useToSetArrayKey($this, $context, $array, $val);
        } else {
            return $array->addArrayKey($this, $context, $val);
        }
    }

    public function unparse():\PhpParser\Node\Expr\ArrayItem {
        return new \PhpParser\Node\Expr\ArrayItem(
            $this->value->unparseExpr(),
            $this->key ? $this->key->unparseExpr() : null,
            $this->byRef
        );
    }
}

class Closure extends Expr {
    /** @var bool */
    private $static;
    /** @var Function_\Function_ */
    private $type;
    /** @var ClosureUse[] */
    private $uses;
    /** @var Stmt\Block */
    private $body;

    /**
     * @param HasCodeLoc          $loc
     * @param bool                $static
     * @param Function_\Function_ $type
     * @param ClosureUse[]        $uses
     * @param Stmt\Block          $body
     */
    public function __construct(
        HasCodeLoc $loc,
        bool $static,
        Function_\Function_ $type,
        array $uses,
        Stmt\Block $body
    ) {
        parent::__construct($loc);
        $this->static = $static;
        $this->type   = $type;
        $this->uses   = $uses;
        $this->body   = $body;
    }

    public function subStmts(bool $deep):array {
        $stmts = $this->type->subStmts();
        if ($deep) {
            $stmts[] = $this->body;
        }
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

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        return new Type\Class_($this, 'Closure');
    }
}

class ClosureUse extends Node {
    /** @var string */
    private $name;
    /** @var bool */
    private $byRef;

    public function __construct(HasCodeLoc $loc, string $name, bool $byRef) {
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

    public function __construct(HasCodeLoc $loc, Expr $cond, Expr $true = null, Expr $false) {
        parent::__construct($loc);
        $this->cond  = $cond;
        $this->true  = $true;
        $this->false = $false;
    }

    public function subStmts(bool $deep):array {
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

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        // TODO Apply type gaurds in $this->cond

        if ($this->true) {
            $true = $this->true->checkExpr($context, $noErrors);
        } else {
            $true = $this->cond->checkExpr($context, $noErrors)->removeFalsy($context);
        }

        $false = $this->false->checkExpr($context, $noErrors);

        return Type\Type::union($this, [$true, $false,]);
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
     * @param HasCodeLoc $loc
     * @param Expr[]     $exprs
     */
    public function __construct(HasCodeLoc $loc, array $exprs) {
        parent::__construct($loc);
        $this->exprs = $exprs;
    }

    public function subStmts(bool $deep):array {
        return $this->exprs;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Scalar\Encapsed(self::unparseEncaps($this->exprs));
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        // TODO Make this really smart and return a SingleValue (or union thereof) if it can
        return new Type\String_($this);
    }
}

class Isset_ extends Expr {
    /** @var Expr[] */
    private $exprs;

    /**
     * @param HasCodeLoc $loc
     * @param Expr[]     $exprs
     */
    public function __construct(HasCodeLoc $loc, array $exprs) {
        parent::__construct($loc);
        $this->exprs = $exprs;
    }

    public function subStmts(bool $deep):array {
        return $this->exprs;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $exprs = [];
        foreach ($this->exprs as $expr) {
            $exprs[] = $expr->unparseExpr();
        }
        return new \PhpParser\Node\Expr\Isset_($exprs);
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        return new Type\Int_($this);
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

    public function __construct(HasCodeLoc $loc, Expr $left, string $type, Expr $right) {
        parent::__construct($loc);
        $this->left  = $left;
        $this->type  = $type;
        $this->right = $right;
    }

    public function subStmts(bool $deep):array {
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

    protected function inferLocals_(Context\Context $context):array {
        switch ($this->type) {
            case self::ASSIGN:
            case self::ASSIGN_REF:
                return $this->left->inferLocal($this->right->checkExpr($context, true), $context);
            default:
                return [];
        }
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        switch ($this->type) {
            case self:: INSTANCEOF:
                return Type\Type::bool($this);

            case self::ADD:
            case self::SUBTRACT:
            case self::MULTIPLY:
            case self::DIVIDE:
            case self::MODULUS:
            case self::EXPONENT:
                return Type\Type::number($this);

            case self::BIT_AND:
            case self::BIT_OR:
            case self::BIT_XOR:
            case self::SHIFT_LEFT:
            case self::SHIFT_RIGHT:
                return new Type\Int_($this);

            case self::EQUAL:
            case self::IDENTICAL:
            case self::NOT_EQUAL:
            case self::NOT_IDENTICAL:
            case self::GREATER:
            case self::LESS:
            case self::GREATER_OR_EQUAL:
            case self::LESS_OR_EQUAL:
                return Type\Type::bool($this);

            case self::SPACESHIP:
                return Type\Type::union($this, [
                    new Type\SingleValue($this, -1),
                    new Type\SingleValue($this, 0),
                    new Type\SingleValue($this, 1),
                ]);

            case self::COALESCE:
                $left  = $this->left->checkExpr($context, $noErrors)->removeNull($context);
                $right = $this->right->checkExpr($context, $noErrors);
                return Type\Type::union($this, [$left, $right]);

            case self::BOOl_AND:
            case self::BOOl_OR:
            case self::LOGIC_AND:
            case self::LOGIC_OR:
            case self::LOGIC_XOR:
                return Type\Type::bool($this);

            case self::CONCAT:
                // TODO Make this be really smart and return a SingleValue if it can
                return new Type\String_($this);

            case self::ASSIGN:
            case self::ASSIGN_REF:
                return $this->right->checkExpr($context, $noErrors);

            case self::ASSIGN_ADD:
            case self::ASSIGN_SUBTRACT:
            case self::ASSIGN_MULTIPLY:
            case self::ASSIGN_DIVIDE:
            case self::ASSIGN_MODULUS:
            case self::ASSIGN_EXPONENT:
                return Type\Type::number($this);

            case self::ASSIGN_CONCAT:
                return new Type\String_($this);

            case self::ASSIGN_BIT_AND:
            case self::ASSIGN_BIT_OR:
            case self::ASSIGN_BIT_XOR:
            case self::ASSIGN_SHIFT_LEFT:
            case self::ASSIGN_SHIFT_RIGHT:
                return new Type\Int_($this);

            default:
                throw new \Exception('huh?');
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

    public function __construct(HasCodeLoc $loc, string $type, Expr $expr) {
        parent::__construct($loc);
        $this->type = $type;
        $this->expr = $expr;
    }

    public function subStmts(bool $deep):array {
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

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        switch ($this->type) {
            case self::INT:
                return new Type\Int_($this);
            case self::BOOL:
                return Type\Type::bool($this);
            case self::FLOAT:
                return new Type\Float_($this);
            case self::STRING:
                return new Type\String_($this);
            case self::ARRAY:
                return new Type\Array_($this, new Type\Mixed($this));
            case self::OBJECT:
                return new Type\Object($this);
            case self::UNSET:
                return new Type\SingleValue($this, null);
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

    public function __construct(HasCodeLoc $loc, string $type, Expr $expr) {
        parent::__construct($loc);
        $this->type = $type;
        $this->expr = $expr;
    }

    public function subStmts(bool $deep):array {
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

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        switch ($this->type) {
            case self::PRE_INC:
            case self::PRE_DEC:
                return Type\Type::number($this);

            case self::POST_INC:
            case self::POST_DEC:
                return $this->expr->checkExpr($context, $noErrors);

            case self::PRINT:
                // "print ..." always evaluates to int(1)
                return new Type\SingleValue($this, 1);

            case self::BOOL_NOT:
                return Type\Type::bool($this);

            case self::BIT_NOT:
                return new Type\Int_($this);

            case self::PLUS:
            case self::NEGATE:
                return Type\Type::number($this);

            case self::SUPPRESS:
                return $this->expr->checkExpr($context, $noErrors);

            case self::EMPTY:
                return Type\Type::bool($this);

            case self::EVAL:
                return new Type\Mixed($this);

            case self::CLONE:
                return $this->expr->checkExpr($context, $noErrors);

            default:
                throw new \Exception('Invalid unary operator type: ' . $this->type);
        }
    }
}

class Exit_ extends Expr {
    /** @var Expr|null */
    private $expr;

    public function __construct(HasCodeLoc $loc, Expr $expr = null) {
        parent::__construct($loc);
        $this->expr = $expr;
    }

    public function subStmts(bool $deep):array {
        return $this->expr ? [$this->expr] : [];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $expr = $this->expr ? $this->expr->unparseExpr() : null;
        return new \PhpParser\Node\Expr\Exit_($expr);
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        // "exit" is an expression??? How do you use the result?
        return Type\Type::none($this);
    }
}

class ShellExec extends Expr {
    /** @var Expr[] */
    private $parts;

    /**
     * @param HasCodeLoc $loc
     * @param Expr[]     $parts
     */
    public function __construct(HasCodeLoc $loc, array $parts) {
        parent::__construct($loc);
        $this->parts = $parts;
    }

    public function subStmts(bool $deep):array {
        return $this->parts;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\ShellExec(ConcatMany::unparseEncaps($this->parts));
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        return new Type\String_($this);
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

    public function __construct(HasCodeLoc $loc, string $class) {
        parent::__construct($loc);
        if (substr($class, 0, 1) === '\\') {
            throw new \Exception("Illegal class name: $class");
        }
        $this->class = $class;
    }

    public function toType(string $static = null, bool $strict = false):Type\Type {
        return new Type\Class_($this, $this->class);
    }

    public function toString(string $static = null):string {
        return $this->class;
    }

    public function subStmts(bool $deep):array {
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

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        return new Type\SingleValue($this, $this->class);
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
        $type = new Type\Class_($this, $this->toString($static));
        $type = new Type\TypeVar($this, Type\TypeVar::STATIC, $type);
        return $type;
    }

    public function subStmts(bool $deep):array {
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

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        return new Type\String_($this);
    }
}
