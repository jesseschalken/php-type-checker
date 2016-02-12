<?php

namespace JesseSchalken\PhpTypeChecker\Constants;

use JesseSchalken\PhpTypeChecker\ErrorReceiver;
use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Decls;
use JesseSchalken\PhpTypeChecker\Type;

class GetConstant extends Expr\Expr {
    /** @var string */
    private $name;

    public function __construct(HasCodeLoc $loc, string $name) {
        parent::__construct($loc);
        $this->name = $name;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\ConstFetch(new \PhpParser\Node\Name\FullyQualified($this->name));
    }

    public function typeCheckExpr(Decls\LocalDecls $locals, Decls\GlobalDecls $globals, ErrorReceiver $errors):Type\Type {
    }
}

class GetClassConstant extends Expr\Expr {
    /** @var Expr\Expr */
    private $class;
    /** @var string */
    private $const;

    public function __construct(HasCodeLoc $loc, Expr\Expr $class, string $const) {
        parent::__construct($loc);
        $this->class = $class;
        $this->const = $const;
    }

    public function subStmts():array {
        return [$this->class];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return new \PhpParser\Node\Expr\ClassConstFetch(
            $this->class->unparseExprOrName(),
            $this->const
        );
    }

    public function typeCheckExpr(Decls\LocalDecls $locals, Decls\GlobalDecls $globals, ErrorReceiver $errors):Type\Type {
        $expr = $globals->getConstant($this->const);
        if (!$expr) {
            $errors->add("Undefined constant '$this->const'", $this);
            return new Type\Mixed($this);
        } else {
            return $expr->typeCheckExpr($locals, $globals, $errors);
        }
    }
}

final class MagicConst extends \JesseSchalken\Enum\StringEnum {
    const LINE      = '__LINE__';
    const FILE      = '__FILE__';
    const DIR       = '__DIR__';
    const FUNCTION  = '__FUNCTION__';
    const CLASS_    = '__CLASS__';
    const TRAIT     = '__TRAIT__';
    const METHOD    = '__METHOD__';
    const NAMESPACE = '__NAMESPACE__';

    public static function values() {
        return [
            self::LINE,
            self::FILE,
            self::DIR,
            self::FUNCTION,
            self::CLASS_,
            self::TRAIT,
            self::METHOD,
            self::NAMESPACE,
        ];
    }
}

class GetMagicConst extends Expr\Expr {
    /** @var string */
    private $type;
    /** @var int|string */
    private $value;

    /**
     * @param HasCodeLoc $loc
     * @param string     $type
     * @param int|string $value
     */
    public function __construct(HasCodeLoc $loc, string $type, $value) {
        parent::__construct($loc);
        $this->type  = $type;
        $this->value = $value;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        switch ($this->type) {
            case MagicConst::LINE:
                return new \PhpParser\Node\Scalar\MagicConst\Line();
            case MagicConst::FILE:
                return new \PhpParser\Node\Scalar\MagicConst\File();
            case MagicConst::DIR:
                return new \PhpParser\Node\Scalar\MagicConst\Dir();
            case MagicConst::FUNCTION:
                return new \PhpParser\Node\Scalar\MagicConst\Function_();
            case MagicConst::CLASS_:
                return new \PhpParser\Node\Scalar\MagicConst\Class_();
            case MagicConst::TRAIT:
                return new \PhpParser\Node\Scalar\MagicConst\Trait_();
            case MagicConst::METHOD:
                return new \PhpParser\Node\Scalar\MagicConst\Method();
            case MagicConst::NAMESPACE:
                return new \PhpParser\Node\Scalar\MagicConst\Namespace_();
            default:
                throw new \Exception('Invlaid magic constant type: ' . $this->type);
        }
    }

    public function typeCheckExpr(Decls\LocalDecls $locals, Decls\GlobalDecls $globals, ErrorReceiver $errors):Type\Type {
        return new Type\SingleValue($this, $this->value);
    }
}

class Literal extends Expr\Expr {
    private static function literalToNode($value):\PhpParser\Node\Expr {
        if (is_string($value)) {
            return new \PhpParser\Node\Scalar\String_($value);
        } elseif (is_bool($value)) {
            $constant = $value ? 'true' : 'false';
            return new \PhpParser\Node\Expr\ConstFetch(new \PhpParser\Node\Name\FullyQualified($constant));
        } elseif (is_float($value)) {
            return new \PhpParser\Node\Scalar\DNumber($value);
        } elseif (is_int($value)) {
            return new \PhpParser\Node\Scalar\LNumber($value);
        } elseif (is_null($value)) {
            return new \PhpParser\Node\Expr\ConstFetch(new \PhpParser\Node\Name\FullyQualified('null'));
        } elseif (is_array($value)) {
            $items = [];
            foreach ($value as $k => $v) {
                $items[] = new \PhpParser\Node\Expr\ArrayItem(
                    self::literalToNode($v),
                    self::literalToNode($k),
                    false
                );
            }
            return new \PhpParser\Node\Expr\Array_($items);
        } else {
            throw new \Exception('Invalid literal type: ' . gettype($value));
        }
    }

    /** @var bool|float|int|null|string */
    private $value;

    /**
     * @param HasCodeLoc                 $loc
     * @param string|int|float|bool|null $value
     */
    public function __construct(HasCodeLoc $loc, $value) {
        parent::__construct($loc);
        $this->value = $value;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseExpr():\PhpParser\Node\Expr {
        return self::literalToNode($this->value);
    }

    public function unparseExprOrString() {
        $value = $this->value;
        if (is_string($value)) {
            return $value;
        } else {
            return parent::unparseExprOrString();
        }
    }

    public function unparseExprOrName() {
        if (is_string($this->value)) {
            return new \PhpParser\Node\Name\FullyQualified($this->value);
        } else {
            return parent::unparseExprOrName();
        }
    }

    public function value() {
        return $this->value;
    }

    public function typeCheckExpr(Decls\LocalDecls $locals, Decls\GlobalDecls $globals, ErrorReceiver $errors):Type\Type {
        return new Type\SingleValue($this, $this->value);
    }
}

