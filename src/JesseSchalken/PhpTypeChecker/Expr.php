<?php

namespace JesseSchalken\PhpTypeChecker\Expr;

use JesseSchalken\PhpTypeChecker\Context;
use JesseSchalken\PhpTypeChecker\Function_;
use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Stmt;
use JesseSchalken\PhpTypeChecker\Type;
use function JesseSchalken\PhpTypeChecker\merge_types;

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
            return $key->checkExpr($context, $noErrors)->useToSetArrayKey($this, $context, $array, $val, $noErrors);
        } else {
            return $array->addArrayKey($this, $context, $val, $noErrors);
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

abstract class AbstractBinOp extends Expr {
    /** @var Expr */
    protected $left;
    /** @var Expr */
    protected $right;

    public function __construct(HasCodeLoc $loc, Expr $left, Expr $right) {
        parent::__construct($loc);
        $this->left  = $left;
        $this->right = $right;
    }

    public function subStmts(bool $deep):array {
        return [$this->left, $this->right];
    }
}

class BinOpType extends \JesseSchalken\Enum\StringEnum {
    const ADD      = '+';
    const SUBTRACT = '-';
    const MULTIPLY = '*';
    const DIVIDE   = '/';
    const MODULUS  = '%';
    const EXPONENT = '**';

    const BIT_AND     = '&';
    const BIT_OR      = '|';
    const BIT_XOR     = '^';
    const SHIFT_LEFT  = '<<';
    const SHIFT_RIGHT = '>>';

    const CONCAT = '.';

    public static function values() {
        return [
            self::ADD,
            self::SUBTRACT,
            self::MULTIPLY,
            self::DIVIDE,
            self::MODULUS,
            self::EXPONENT,
            self::BIT_AND,
            self::BIT_OR,
            self::BIT_XOR,
            self::SHIFT_LEFT,
            self::SHIFT_RIGHT,
            self::CONCAT,
        ];
    }

    public function __toString() {
        return $this->value();
    }

    public function evaluate($lhs, $rhs) {
        switch ($this->value()) {
            case self::ADD:
                return $lhs + $rhs;
            case self::SUBTRACT:
                return $lhs - $rhs;
            case self::MULTIPLY:
                return $lhs * $rhs;
            case self::DIVIDE:
                return $lhs / $rhs;
            case self::MODULUS:
                return $lhs % $rhs;
            case self::EXPONENT:
                return $lhs ** $rhs;
            case self::BIT_AND:
                return $lhs & $rhs;
            case self::BIT_OR:
                return $lhs | $rhs;
            case self::BIT_XOR:
                return $lhs ^ $rhs;
            case self::SHIFT_LEFT:
                return $lhs << $rhs;
            case self::SHIFT_RIGHT:
                return $lhs >> $rhs;
            case self::CONCAT:
                return $lhs . $rhs;
            default:
                throw new \Exception("Invalid binary op: $this");
        }
    }
}

class LogicalOpType extends \JesseSchalken\Enum\StringEnum {
    const BOOl_AND  = '&&';
    const BOOl_OR   = '||';
    const LOGIC_AND = 'and';
    const LOGIC_OR  = 'or';
    const LOGIC_XOR = 'xor';

    public static function values() {
        return [
            self::BOOl_AND,
            self::BOOl_OR,
            self::LOGIC_AND,
            self::LOGIC_OR,
            self::LOGIC_XOR,
        ];
    }

    public function __toString() {
        return $this->value();
    }
}

class AssignOp extends AbstractBinOp {
    /** @var BinOpType */
    private $type;

    public function __construct(HasCodeLoc $loc, Expr $right, BinOpType $type, Expr $left) {
        parent::__construct($loc, $left, $right);
        $this->type = $type;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $left  = $this->left->unparseExpr();
        $right = $this->right->unparseExpr();
        switch ($this->type->value()) {
            case BinOpType::ADD:
                return new \PhpParser\Node\Expr\AssignOp\Plus($left, $right);
            case BinOpType::SUBTRACT:
                return new \PhpParser\Node\Expr\AssignOp\Minus($left, $right);
            case BinOpType::MULTIPLY:
                return new \PhpParser\Node\Expr\AssignOp\Mul($left, $right);
            case BinOpType::DIVIDE:
                return new \PhpParser\Node\Expr\AssignOp\Div($left, $right);
            case BinOpType::MODULUS:
                return new \PhpParser\Node\Expr\AssignOp\Mod($left, $right);
            case BinOpType::EXPONENT:
                return new \PhpParser\Node\Expr\AssignOp\Pow($left, $right);
            case BinOpType::CONCAT:
                return new \PhpParser\Node\Expr\AssignOp\Concat($left, $right);
            case BinOpType::BIT_AND:
                return new \PhpParser\Node\Expr\AssignOp\BitwiseAnd($left, $right);
            case BinOpType::BIT_OR:
                return new \PhpParser\Node\Expr\AssignOp\BitwiseOr($left, $right);
            case BinOpType::BIT_XOR:
                return new \PhpParser\Node\Expr\AssignOp\BitwiseXor($left, $right);
            case BinOpType::SHIFT_LEFT:
                return new \PhpParser\Node\Expr\AssignOp\ShiftLeft($left, $right);
            case BinOpType::SHIFT_RIGHT:
                return new \PhpParser\Node\Expr\AssignOp\ShiftRight($left, $right);
            default:
                throw new \Exception('Invalid binary operator type: ' . $this->type->value());
        }
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        $lhs = $this->left->checkExpr($context, $noErrors);
        $rhs = $this->right->checkExpr($context, $noErrors);
        $val = $lhs->doBinOp($this, $rhs, $this->type, $context, $noErrors);
        if (!$noErrors) {
            $lhs->checkContains($this, $val, $context);
        }
        // The result of an assignment operation is the value assigned back to the LHS
        return $val;
    }
}

class Assign extends Expr {
    /** @var Expr */
    private $left;
    /** @var Expr */
    private $right;
    /** @var bool */
    private $byRef;

    public function __construct(HasCodeLoc $loc, Expr $left, Expr $right, $byRef) {
        parent::__construct($loc);
        $this->left  = $left;
        $this->right = $right;
        $this->byRef = $byRef;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $left  = $this->left->unparseExpr();
        $right = $this->right->unparseExpr();
        if ($this->byRef) {
            return new \PhpParser\Node\Expr\AssignRef($left, $right);
        } else {
            return new \PhpParser\Node\Expr\Assign($left, $right);
        }
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        $right = $this->right->checkExpr($context, $noErrors);
        if (!$noErrors) {
            $left = $this->left->checkExpr($context, $noErrors);
            if ($this->byRef) {
                $left->checkEquivelant($this, $right, $context);
            } else {
                $left->checkContains($this, $right, $context);
            }
        }
        return $right;
    }

    protected function inferLocals(Context\Context $context):array {
        return merge_types(
            parent::inferLocals($context),
            $this->left->inferLocal($this->right->checkExpr($context, true), $context),
            $context
        );
    }

    public function subStmts(bool $deep):array {
        return [$this->left, $this->right];
    }
}

class InstanceOf_ extends AbstractBinOp {
    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\Instanceof_(
            $this->left->unparseExpr(),
            $this->right->unparseExprOrName()
        );
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        // TODO do some actual checking
        // TODO maybe return "true" or "false" single values if the LHS is known to be/not to be an instanceof RHS
        return Type\Type::bool($this);
    }
}

class LogicalOp extends AbstractBinOp {
    private $type;

    public function __construct(HasCodeLoc $loc, Expr $left, LogicalOpType $type, Expr $right) {
        parent::__construct($loc, $left, $right);
        $this->type = $type;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $left  = $this->left->unparseExpr();
        $right = $this->right->unparseExpr();
        switch ($this->type->value()) {
            case LogicalOpType::BOOl_AND:
                return new \PhpParser\Node\Expr\BinaryOp\BooleanAnd($left, $right);
            case LogicalOpType::BOOl_OR:
                return new \PhpParser\Node\Expr\BinaryOp\BooleanOr($left, $right);
            case LogicalOpType::LOGIC_AND:
                return new \PhpParser\Node\Expr\BinaryOp\LogicalAnd($left, $right);
            case LogicalOpType::LOGIC_OR:
                return new \PhpParser\Node\Expr\BinaryOp\LogicalOr($left, $right);
            case LogicalOpType::LOGIC_XOR:
                return new \PhpParser\Node\Expr\BinaryOp\LogicalXor($left, $right);
            default:
                throw new \Exception('huh?');
        }
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        // TODO do some actual checking
        // TODO maybe return "true" or "false" single values if the value is known
        return Type\Type::bool($this);
    }
}

class Coalesce extends AbstractBinOp {
    public function unparseExpr():\PhpParser\Node\Expr {
        $left  = $this->left->unparseExpr();
        $right = $this->right->unparseExpr();
        return new \PhpParser\Node\Expr\BinaryOp\Coalesce($left, $right);
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        $left  = $this->left->checkExpr($context, $noErrors)->removeNull($context);
        $right = $this->right->checkExpr($context, $noErrors);
        return Type\Type::union($this, [$left, $right]);
    }
}

class Spaceship extends AbstractBinOp {
    public function unparseExpr():\PhpParser\Node\Expr {
        $left  = $this->left->unparseExpr();
        $right = $this->right->unparseExpr();
        return new \PhpParser\Node\Expr\BinaryOp\Spaceship($left, $right);
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        // TODO do some actual checking here
        return Type\Type::union($this, [
            new Type\SingleValue($this, -1),
            new Type\SingleValue($this, 0),
            new Type\SingleValue($this, 1),
        ]);
    }
}

class ComparisonOpType extends \JesseSchalken\Enum\StringEnum {
    const EQUAL            = '==';
    const IDENTICAL        = '===';
    const NOT_EQUAL        = '!=';
    const NOT_IDENTICAL    = '!==';
    const GREATER          = '>';
    const LESS             = '<';
    const GREATER_OR_EQUAL = '>=';
    const LESS_OR_EQUAL    = '<=';

    public static function values() {
        return [
            self::EQUAL,
            self::IDENTICAL,
            self::NOT_EQUAL,
            self::NOT_IDENTICAL,
            self::GREATER,
            self::LESS,
            self::GREATER_OR_EQUAL,
            self::LESS_OR_EQUAL,
        ];
    }

    public function __toString() {
        return $this->value();
    }
}

class Comparison extends AbstractBinOp {
    /** @var ComparisonOpType */
    private $type;

    public function __construct(HasCodeLoc $loc, Expr $left, Expr $right, ComparisonOpType $type) {
        parent::__construct($loc, $left, $right);
        $this->type = $type;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $left  = $this->left->unparseExpr();
        $right = $this->right->unparseExpr();
        switch ($this->type->value()) {
            case ComparisonOpType::EQUAL:
                return new \PhpParser\Node\Expr\BinaryOp\Equal($left, $right);
            case ComparisonOpType::IDENTICAL:
                return new \PhpParser\Node\Expr\BinaryOp\Identical($left, $right);
            case ComparisonOpType::NOT_EQUAL:
                return new \PhpParser\Node\Expr\BinaryOp\NotEqual($left, $right);
            case ComparisonOpType::NOT_IDENTICAL:
                return new \PhpParser\Node\Expr\BinaryOp\NotIdentical($left, $right);
            case ComparisonOpType::GREATER:
                return new \PhpParser\Node\Expr\BinaryOp\Greater($left, $right);
            case ComparisonOpType::LESS:
                return new \PhpParser\Node\Expr\BinaryOp\Smaller($left, $right);
            case ComparisonOpType::GREATER_OR_EQUAL:
                return new \PhpParser\Node\Expr\BinaryOp\GreaterOrEqual($left, $right);
            case ComparisonOpType::LESS_OR_EQUAL:
                return new \PhpParser\Node\Expr\BinaryOp\SmallerOrEqual($left, $right);
            default:
                throw new \Exception("huh? $this->type");
        }
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        // TODO do some actual checking
        // TODO should probably just check the lhs/rhs against int|bool|float
        return Type\Type::bool($this);
    }
}

class BinOp extends AbstractBinOp {
    /** @var BinOpType */
    private $type;

    public function __construct(HasCodeLoc $loc, Expr $left, BinOpType $type, Expr $right) {
        parent::__construct($loc, $left, $right);
        $this->type = $type;
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $left  = $this->left->unparseExpr();
        $right = $this->right->unparseExpr();
        switch ($this->type) {
            case BinOpType::ADD:
                return new \PhpParser\Node\Expr\BinaryOp\Plus($left, $right);
            case BinOpType::SUBTRACT:
                return new \PhpParser\Node\Expr\BinaryOp\Minus($left, $right);
            case BinOpType::MULTIPLY:
                return new \PhpParser\Node\Expr\BinaryOp\Mul($left, $right);
            case BinOpType::DIVIDE:
                return new \PhpParser\Node\Expr\BinaryOp\Div($left, $right);
            case BinOpType::MODULUS:
                return new \PhpParser\Node\Expr\BinaryOp\Mod($left, $right);
            case BinOpType::EXPONENT:
                return new \PhpParser\Node\Expr\BinaryOp\Pow($left, $right);
            case BinOpType::BIT_AND:
                return new \PhpParser\Node\Expr\BinaryOp\BitwiseAnd($left, $right);
            case BinOpType::BIT_OR:
                return new \PhpParser\Node\Expr\BinaryOp\BitwiseOr($left, $right);
            case BinOpType::BIT_XOR:
                return new \PhpParser\Node\Expr\BinaryOp\BitwiseXor($left, $right);
            case BinOpType::SHIFT_LEFT:
                return new \PhpParser\Node\Expr\BinaryOp\ShiftLeft($left, $right);
            case BinOpType::SHIFT_RIGHT:
                return new \PhpParser\Node\Expr\BinaryOp\ShiftRight($left, $right);
            case BinOpType::CONCAT:
                return new \PhpParser\Node\Expr\BinaryOp\Concat($left, $right);
            default:
                throw new \Exception('Invalid binary operator type: ' . $this->type);
        }
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        $left  = $this->left->checkExpr($context, $noErrors);
        $right = $this->right->checkExpr($context, $noErrors);
        return $left->doBinOp($this, $right, $this->type, $context, $noErrors);
    }
}

class CastType extends \JesseSchalken\Enum\StringEnum {
    const INT    = 'int';
    const BOOL   = 'bool';
    const FLOAT  = 'float';
    const STRING = 'string';
    const ARRAY  = 'array';
    const OBJECT = 'object';
    const UNSET  = 'unset';

    public static function values() {
        return [
            self::INT,
            self::BOOL,
            self::FLOAT,
            self::ARRAY,
            self::OBJECT,
            self::UNSET,
        ];
    }

    public function toType(HasCodeLoc $loc):Type\Type {
        switch ($this->value()) {
            case self::INT:
                return new Type\Int_($loc);
            case self::BOOL:
                return Type\Type::bool($loc);
            case self::FLOAT:
                return new Type\Float_($loc);
            case self::STRING:
                return new Type\String_($loc);
            case self::ARRAY:
                return new Type\Array_($loc, new Type\Mixed($loc));
            case self::OBJECT:
                return new Type\Object($loc);
            case self::UNSET:
                return new Type\SingleValue($loc, null);
            default:
                throw new \Exception('Invalid cast type: ' . $this);
        }
    }

    public function evaluate($value) {
        switch ($this->value()) {
            case self::INT:
                return (int)$value;
            case self::BOOL:
                return (bool)$value;
            case self::FLOAT:
                return (float)$value;
            case self::STRING:
                return (string)$value;
            case self::ARRAY:
                return (array)$value;
            case self::OBJECT:
                return (object)$value;
            case self::UNSET:
                return (unset)$value;
            default:
                throw new \Exception('Invalid cast type: ' . $this);
        }
    }

    public function __toString() {
        return $this->value();
    }
}

class Cast extends Expr {
    /** @var string */
    private $type;
    /** @var CastType */
    private $expr;

    public function __construct(HasCodeLoc $loc, CastType $type, Expr $expr) {
        parent::__construct($loc);
        $this->type = $type;
        $this->expr = $expr;
    }

    public function subStmts(bool $deep):array {
        return [$this->expr];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        $expr = $this->expr->unparseExpr();
        switch ($this->type->value()) {
            case CastType::INT:
                return new \PhpParser\Node\Expr\Cast\Int_($expr);
            case CastType::BOOL:
                return new \PhpParser\Node\Expr\Cast\Bool_($expr);
            case CastType::FLOAT:
                return new \PhpParser\Node\Expr\Cast\Double($expr);
            case CastType::STRING:
                return new \PhpParser\Node\Expr\Cast\String_($expr);
            case CastType::ARRAY:
                return new \PhpParser\Node\Expr\Cast\Array_($expr);
            case CastType::OBJECT:
                return new \PhpParser\Node\Expr\Cast\Object_($expr);
            case CastType::UNSET:
                return new \PhpParser\Node\Expr\Cast\Unset_($expr);
            default:
                throw new \Exception('Invalid cast type: ' . $this->type);
        }
    }

    public function checkExpr(Context\Context $context, bool $noErrors):Type\Type {
        return $this->expr->checkExpr($context, $noErrors)->doCast($this, $this->type, $context, $noErrors);
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
