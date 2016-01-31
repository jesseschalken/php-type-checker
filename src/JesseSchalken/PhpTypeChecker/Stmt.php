<?php

namespace JesseSchalken\PhpTypeChecker\Stmt;

use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\Expr;
use function JesseSchalken\PhpTypeChecker\extract_namespace;
use JesseSchalken\PhpTypeChecker\Node;
use function JesseSchalken\PhpTypeChecker\remove_namespace;
use JesseSchalken\PhpTypeChecker\Type;
use JesseSchalken\PhpTypeChecker\Parser;
use function JesseSchalken\MagicUtils\clone_ref;
use function JesseSchalken\PhpTypeChecker\recursive_scan2;

abstract class Stmt extends Node {
    /**
     * @return SingleStmt[]
     */
    public abstract function split():array;

    /**
     * @return Stmt[]
     */
    public abstract function subStmts():array;

    public final function namespaces():array {
        $namespaces = [];
        foreach ($this->findDeclarations() as $decl) {
            $namespaces[] = $decl->namespace_();
        }
        return array_unique($namespaces);
    }

    /**
     * @return Declaration[]
     */
    public function findDeclarations():array {
        $decls = [];
        foreach ($this->subStmts() as $stmt) {
            foreach ($stmt->findDeclarations() as $decl) {
                $decls[] = $decl;
            }
        }
        return $decls;
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

abstract class ControlStructure extends SingleStmt {
}

abstract class Declaration extends SingleStmt {
    public abstract function name():string;

    public final function namespace_():string {
        return extract_namespace($this->name());
    }

    public final function findDeclarations():array {
        $decls   = parent::findDeclarations();
        $decls[] = $this;
        return $decls;
    }
}

abstract class VariableType extends SingleStmt {
    /** @var string */
    private $name;
    /** @var Type\Type */
    private $type;

    public function __construct(CodeLoc $loc, string $name, Type\Type $type) {
        parent::__construct($loc);
        $this->name = $name;
        $this->type = $type;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseStmt() {
        return null;
    }
}

class GlobalVariableType extends VariableType {
}

class LocalVariableType extends VariableType {
}

class DoWhile extends ControlStructure {
    /** @var Block */
    private $body;
    /** @var Expr\Expr */
    private $cond;

    /**
     * @param CodeLoc   $loc
     * @param Block     $body
     * @param Expr\Expr $cond
     */
    public function __construct(CodeLoc $loc, Block $body, Expr\Expr $cond) {
        parent::__construct($loc);
        $this->body = $body;
        $this->cond = $cond;
    }

    public function subStmts():array {
        return [$this->body, $this->cond];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\While_(
            $this->cond->unparseExpr(),
            $this->body->unparseNodes()
        );
    }
}

class If_ extends ControlStructure {
    /** @var Expr\Expr */
    private $cond;
    /** @var Block */
    private $true;
    /** @var Block */
    private $false;

    public function __construct(CodeLoc $loc, Expr\Expr $cond, Block $true, Block $false) {
        parent::__construct($loc);
        $this->cond  = $cond;
        $this->true  = $true;
        $this->false = $false;
    }

    public function subStmts():array {
        return [$this->cond, $this->true, $this->false];
    }

    public function unparseStmt() {
        $elseIfs = [];
        $else    = $this->false->unparseNodes();

        if (count($else) == 1) {
            $if_ = $else[0];
            if ($if_ instanceof \PhpParser\Node\Stmt\If_) {
                $elseIfs = array_merge(
                    [new \PhpParser\Node\Stmt\ElseIf_(
                        $if_->cond,
                        $if_->stmts
                    )],
                    $if_->elseifs
                );
                $else    = $if_->else ? $if_->else->stmts : [];
            }
        }

        return new \PhpParser\Node\Stmt\If_(
            $this->cond->unparseExpr(),
            [
                'stmts'   => $this->true->unparseNodes(),
                'elseifs' => $elseIfs,
                'else'    => $else ? new \PhpParser\Node\Stmt\Else_($else) : null,
            ]
        );
    }
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

class Visibility {
    static function unparse(string $visibility):int {
        switch ($visibility) {
            case self::PUBLIC:
                return \PhpParser\Node\Stmt\Class_::MODIFIER_PUBLIC;
            case self::PROTECTED:
                return \PhpParser\Node\Stmt\Class_::MODIFIER_PROTECTED;
            case self::PRIVATE:
                return \PhpParser\Node\Stmt\Class_::MODIFIER_PRIVATE;
            default:
                throw new \Exception('Invalid visibility: ' . $visibility);
        }
    }

    const PUBLIC    = 'public';
    const PROTECTED = 'protected';
    const PRIVATE   = 'private';
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

abstract class Classish extends Declaration {
    private $name;

    public function __construct(CodeLoc $loc, string $name) {
        parent::__construct($loc);
        $this->name = $name;
    }

    public function name():string {
        return $this->name;
    }

    public function basename() {
        return remove_namespace($this->name);
    }

    /**
     * @return AbstractClassMember[]
     */
    public abstract function members():array;

    public function subStmts():array {
        $stmts = [];
        foreach ($this->members() as $member) {
            foreach ($member->subStmts() as $stmt) {
                $stmts[] = $stmt;
            }
        }
        return $stmts;
    }

    /**
     * @return \PhpParser\Node[]
     */
    public function unparseMembers():array {
        $nodes = [];
        foreach ($this->members() as $member) {
            $nodes[] = $member->unparse();
        }
        return $nodes;
    }
}

class Trait_ extends Classish {
    /** @var ClassMember[] */
    private $members = [];

    /**
     * @param CodeLoc       $loc
     * @param string        $name
     * @param ClassMember[] $members
     */
    public function __construct(CodeLoc $loc, string $name, array $members) {
        parent::__construct($loc, $name);
        $this->members = $members;
    }

    public function members():array {
        return $this->members;
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Trait_(
            $this->basename(),
            $this->unparseMembers()
        );
    }
}

class Class_ extends Classish {
    /** @var ClassMember */
    private $members = [];
    /** @var string|null */
    private $parent;
    /** @var string[] */
    private $implements;
    /** @var bool */
    private $abstract;
    /** @var bool */
    private $final;

    /**
     * @param CodeLoc       $loc
     * @param string        $name
     * @param ClassMember[] $members
     * @param string|null   $parent
     * @param string[]      $implements
     * @param bool          $abstract
     * @param bool          $final
     */
    public function __construct(
        CodeLoc $loc,
        string $name,
        array $members,
        string $parent = null,
        array $implements = [],
        bool $abstract,
        bool $final
    ) {
        parent::__construct($loc, $name);
        $this->members    = $members;
        $this->parent     = $parent;
        $this->implements = $implements;
        $this->abstract   = $abstract;
        $this->final      = $final;
    }

    public function members():array {
        return $this->members;
    }

    public function unparseStmt() {
        $type = 0;
        if ($this->abstract) {
            $type |= \PhpParser\Node\Stmt\Class_::MODIFIER_ABSTRACT;
        }
        if ($this->final) {
            $type |= \PhpParser\Node\Stmt\Class_::MODIFIER_FINAL;
        }
        $implements = [];
        foreach ($this->implements as $interface) {
            $implements[] = new \PhpParser\Node\Name\FullyQualified($interface);
        }
        return new \PhpParser\Node\Stmt\Class_(
            $this->basename(),
            [
                'type'       => $type,
                'extends'    => $this->parent ? new \PhpParser\Node\Name\FullyQualified($this->parent) : null,
                'implements' => $implements,
                'stmts'      => $this->unparseMembers(),
            ]
        );
    }
}

class Interface_ extends Classish {
    /** @var Method_[] */
    private $methods = [];
    /** @var string[] */
    private $extends = [];

    /**
     * @param CodeLoc   $loc
     * @param string    $name
     * @param string[]  $extends
     * @param Method_[] $methods
     */
    public function __construct(CodeLoc $loc, string $name, array $extends, array $methods) {
        parent::__construct($loc, $name);
        $this->methods = $methods;
        $this->extends = $extends;
    }

    public function members():array {
        return $this->methods;
    }

    public function unparseStmt() {
        $extends = [];
        foreach ($this->extends as $extend) {
            $extends[] = new \PhpParser\Node\Name\FullyQualified($extend);
        }
        return new \PhpParser\Node\Stmt\Interface_(
            $this->basename(),
            [
                'stmts'   => $this->unparseMembers(),
                'extends' => $extends,
            ]
        );
    }
}

class Function_ extends Declaration {
    /** @var string */
    private $name;
    /** @var Block|null */
    private $body;
    /** @var FunctionSignature */
    private $type;

    public function __construct(CodeLoc $loc, string $name, FunctionSignature $type, Block $body = null) {
        parent::__construct($loc);
        $this->name = $name;
        $this->type = $type;
        $this->body = $body;
    }

    public function name():string {
        return $this->name;
    }

    public function subStmts():array {
        $stmts = $this->type->subStmts();
        if ($this->body) {
            $stmts[] = $this->body;
        }
        return $stmts;
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Function_(
            remove_namespace($this->name),
            array_replace(
                $this->type->unparseAttributes(), [
                'stmts' => $this->body ? $this->body->unparseNodes() : null,
            ]
            )
        );
    }
}

abstract class AbstractClassMember extends Node {
    public abstract function unparse():\PhpParser\Node;

    /**
     * @return Stmt[]
     */
    public abstract function subStmts():array;
}

class ClassConstant extends AbstractClassMember {
    /** @var string */
    private $name;
    /** @var Expr\Expr */
    private $value;

    public function __construct(CodeLoc $loc, string $name, Expr\Expr $value) {
        parent::__construct($loc);
        $this->name  = $name;
        $this->value = $value;
    }

    public function unparse():\PhpParser\Node {
        return new \PhpParser\Node\Stmt\ClassConst(
            [
                new \PhpParser\Node\Const_(
                    $this->name,
                    $this->value->unparseExpr()
                ),
            ]
        );
    }

    public function subStmts():array {
        if ($this->value) {
            return [$this->value];
        } else {
            return [];
        }
    }
}

class UseTrait extends AbstractClassMember {
    /** @var string[] */
    private $traits = [];
    /** @var UseTraitInsteadof[] */
    private $insteadOfs = [];
    /** @var UseTraitAlias[] */
    private $aliases = [];

    /**
     * @param CodeLoc  $loc
     * @param string[] $traits
     */
    public function __construct(CodeLoc $loc, array $traits) {
        parent::__construct($loc);
        $this->traits = $traits;
    }

    public function addInsteadOf(UseTraitInsteadof $insteadof) {
        $lower = strtolower($insteadof->method());
        if (isset($this->insteadOfs[$lower])) {
            throw new \Exception('An *insteadof* already exists for method ' . $insteadof->method());
        }
        $this->insteadOfs[$lower] = $insteadof;
    }

    public function addAlias(UseTraitAlias $alias) {
        $lower = strtolower($alias->alias());
        if (isset($this->aliases[$lower])) {
            throw new \Exception('An *as* already exists for method ' . $alias->alias());
        }
        $this->aliases[$lower] = $alias;
    }

    public function subStmts():array {
        return [];
    }

    public function unparse():\PhpParser\Node {
        $traits = [];
        foreach ($this->traits as $trait) {
            $traits[] = new \PhpParser\Node\Name\FullyQualified($trait);
        }
        $adaptions = [];
        foreach ($this->insteadOfs as $method => $adaption) {
            foreach ($adaption->unparse() as $item) {
                $adaptions[] = $item;
            }
        }
        return new \PhpParser\Node\Stmt\TraitUse($traits, $adaptions);
    }
}

class UseTraitInsteadof extends Node {
    /**
     * The method in question
     * @var string
     */
    private $method;
    /**
     * The trait this method should come from
     * @var string
     */
    private $trait;
    /**
     * The traits this method should *not* come from :P
     * These traits must be used in the class.
     * @var string[]
     */
    private $insteadOf;

    public function __construct(CodeLoc $loc, string $trait, string $method, array $insteadOf) {
        parent::__construct($loc);
        $this->trait     = $trait;
        $this->method    = $method;
        $this->insteadOf = $insteadOf;
    }

    public function method():array {
        return $this->method;
    }

    public function unparse():\PhpParser\Node\Stmt\TraitUseAdaptation\Precedence {
        $insteadOf = [];
        foreach ($this->insteadOf as $trait) {
            $insteadOf[] = new \PhpParser\Node\Name\FullyQualified($trait);
        }
        return new \PhpParser\Node\Stmt\TraitUseAdaptation\Precedence(
            new \PhpParser\Node\Name\FullyQualified($this->trait),
            $this->method,
            $insteadOf
        );
    }
}

class UseTraitAlias extends Node {
    /**
     * The name of the alias
     * @var string
     */
    private $alias;
    /**
     * The method being aliased
     * @var string
     */
    private $method;
    /**
     * The trait the method should come from. If none, use whatever trait implements the method after the
     * "insteadof" rules have been applied.
     * @var string|null
     */
    private $trait;
    /**
     * Any adjustments to visibility. Default to visibility from the trait.
     * @var string|null
     */
    private $visibility;

    /**
     * @param CodeLoc     $loc
     * @param string      $alias
     * @param string      $method
     * @param null|string $trait
     * @param null|string $visibility
     */
    public function __construct(CodeLoc $loc, string $alias, string $method, $trait, $visibility) {
        parent::__construct($loc);
        $this->alias      = $alias;
        $this->method     = $method;
        $this->trait      = $trait;
        $this->visibility = $visibility;
    }

    public function alias():string {
        return $this->alias;
    }

    public function unparse():\PhpParser\Node\Stmt\TraitUseAdaptation\Alias {
        return new \PhpParser\Node\Stmt\TraitUseAdaptation\Alias(
            $this->trait ? new \PhpParser\Node\Name\FullyQualified($this->trait) : null,
            $this->method,
            $this->visibility ? Visibility::unparse($this->visibility) : null,
            $this->alias === $this->method ? null : $this->alias
        );
    }
}

abstract class ClassMember extends AbstractClassMember {
    /** @var string */
    private $visibility;
    /** @var bool */
    private $static;

    /**
     * @param CodeLoc $loc
     * @param string  $visibility
     * @param bool    $static
     */
    public function __construct(CodeLoc $loc, string $visibility, bool $static) {
        parent::__construct($loc);
        $this->visibility = $visibility;
        $this->static     = $static;
    }

    public function visibility():string {
        return $this->visibility;
    }

    public final function modifiers():int {
        $type = 0;
        switch ($this->visibility) {
            case 'public':
                $type |= \PhpParser\Node\Stmt\Class_::MODIFIER_PUBLIC;
                break;
            case 'private';
                $type |= \PhpParser\Node\Stmt\Class_::MODIFIER_PRIVATE;
                break;
            case 'protected';
                $type |= \PhpParser\Node\Stmt\Class_::MODIFIER_PROTECTED;
                break;
        }

        if ($this->static) {
            $type |= \PhpParser\Node\Stmt\Class_::MODIFIER_STATIC;
        }

        return $type;
    }
}

class Property extends ClassMember {
    /** @var string */
    private $name;
    /** @var Type\Type */
    private $type;
    /** @var Expr\Expr|null */
    private $default = null;

    /**
     * @param CodeLoc        $loc
     * @param string         $name
     * @param Type\Type      $type
     * @param Expr\Expr|null $default
     * @param string         $visibility
     * @param bool           $static
     */
    public function __construct(
        CodeLoc $loc,
        string $name,
        Type\Type $type,
        Expr\Expr $default = null,
        string $visibility,
        bool $static
    ) {
        parent::__construct($loc, $visibility, $static);
        $this->name    = $name;
        $this->type    = $type;
        $this->default = $default;
    }

    public function subStmts():array {
        return $this->default ? [$this->default] : [];
    }

    public function unparse():\PhpParser\Node {
        return new \PhpParser\Node\Stmt\Property(
            $this->modifiers(),
            [new \PhpParser\Node\Stmt\PropertyProperty(
                $this->name,
                $this->default ? $this->default->unparseExpr() : null
            )]
        );
    }
}

class FunctionSignature extends Node {
    /** @var bool */
    private $returnRef;
    /** @var Type\Type */
    private $returnType;
    /** @var FunctionParam[] */
    private $params = [];

    /**
     * @param CodeLoc         $loc
     * @param bool            $returnRef
     * @param FunctionParam[] $params
     * @param Type\Type       $returnType
     */
    public function __construct(CodeLoc $loc, bool $returnRef, array $params, Type\Type $returnType) {
        parent::__construct($loc);
        $this->returnRef  = $returnRef;
        $this->params     = $params;
        $this->returnType = $returnType;
    }

    public function subStmts():array {
        $stmts = [];
        foreach ($this->params as $param) {
            foreach ($param->subStmts() as $stmt) {
                $stmts[] = $stmt;
            }
        }
        return $stmts;
    }

    public function unparseAttributes():array {
        $params = [];
        foreach ($this->params as $param) {
            $params[] = $param->unparse();
        }
        return [
            'byRef'      => $this->returnRef,
            'params'     => $params,
            'returnType' => $this->returnType->toTypeHint(),
        ];
    }
}

class Method_ extends ClassMember {
    /** @var string */
    private $name;
    /** @var FunctionSignature */
    private $type;
    /** @var Block|null */
    private $body;
    /** @var bool */
    private $final;

    /**
     * @param CodeLoc           $loc
     * @param string            $name
     * @param FunctionSignature $type
     * @param Block|null        $body
     * @param bool              $final
     * @param string            $visibility
     * @param bool              $static
     */
    public function __construct(
        CodeLoc $loc,
        string $name,
        FunctionSignature $type,
        Block $body = null,
        bool $final,
        string $visibility,
        bool $static
    ) {
        parent::__construct($loc, $visibility, $static);
        $this->final = $final;
        $this->name  = $name;
        $this->type  = $type;
        $this->body  = $body;
    }

    public function isAbstract():bool {
        return $this->body ? false : true;
    }

    public function isFinal():bool {
        return $this->final ? true : false;
    }

    public function subStmts():array {
        $stmts = $this->type->subStmts();
        if ($this->body) {
            $stmts[] = $this->body;
        }
        return $stmts;
    }

    public function unparse():\PhpParser\Node {
        $params = $this->type->unparseAttributes();
        $params = array_replace(
            $params, [
            'type'  => $this->modifiers(),
            'stmts' => $this->body ? $this->body->unparseNodes() : null,
        ]
        );
        return new \PhpParser\Node\Stmt\ClassMethod($this->name, $params);
    }
}

class FunctionParam extends Node {
    /** @var string */
    private $name;
    /** @var Expr\Expr|null */
    private $default = null;
    /** @var bool */
    private $passByRef;
    /** @var bool */
    private $variadic;
    /** @var Type\Type */
    private $type;

    /**
     * @param CodeLoc        $loc
     * @param string         $name
     * @param Expr\Expr|null $default
     * @param bool           $passByRef
     * @param bool           $variadic
     * @param Type\Type      $type
     */
    public function __construct(
        CodeLoc $loc,
        string $name,
        Expr\Expr $default = null,
        bool $passByRef,
        bool $variadic,
        Type\Type $type
    ) {
        parent::__construct($loc);
        $this->name      = $name;
        $this->default   = $default;
        $this->passByRef = $passByRef;
        $this->variadic  = $variadic;
        $this->type      = $type;
    }

    public function subStmts():array {
        return $this->default ? [$this->default] : [];
    }

    public function unparse():\PhpParser\Node\Param {
        return new \PhpParser\Node\Param(
            $this->name,
            $this->default ? $this->default->unparseExpr() : null,
            $this->type->toTypeHint(),
            $this->passByRef,
            $this->variadic
        );
    }
}

class Foreach_ extends ControlStructure {
    /** @var Expr\Expr */
    private $array;
    /** @var Expr\Expr|null */
    private $key;
    /** @var Expr\Expr */
    private $value;
    /** @var Block */
    private $body;
    /** @var bool */
    private $byRef;

    /**
     * @param CodeLoc        $loc
     * @param Expr\Expr      $array
     * @param Expr\Expr|null $key
     * @param Expr\Expr      $value
     * @param bool           $byRef
     * @param Block          $body
     */
    public function __construct(
        CodeLoc $loc,
        Expr\Expr $array,
        Expr\Expr $key = null,
        Expr\Expr $value,
        bool $byRef,
        Block $body
    ) {
        parent::__construct($loc);
        $this->array = $array;
        $this->key   = $key;
        $this->value = $value;
        $this->body  = $body;
        $this->byRef = $byRef;
    }

    public function subStmts():array {
        $stmts = [
            $this->array,
            $this->value,
            $this->body,
        ];
        if ($this->key) {
            $stmts[] = $this->key;
        }
        return $stmts;
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Foreach_(
            $this->array->unparseExpr(),
            $this->value->unparseExpr(),
            [
                'keyVar' => $this->key ? $this->key->unparseExpr() : null,
                'byRef'  => $this->byRef,
                'stmts'  => $this->body->unparseNodes(),
            ]
        );
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

class Const_ extends Declaration {
    /** @var string */
    private $name;
    /** @var Expr\Expr */
    private $value;

    /**
     * @param CodeLoc   $loc
     * @param string    $name
     * @param Expr\Expr $value
     */
    public function __construct(CodeLoc $loc, string $name, Expr\Expr $value) {
        parent::__construct($loc);
        $this->name  = $name;
        $this->value = $value;
    }

    public function name():string {
        return $this->name;
    }

    public function subStmts():array {
        return [$this->value];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Const_(
            [
                new \PhpParser\Node\Const_(
                    remove_namespace($this->name),
                    $this->value->unparseExpr()
                ),
            ]
        );
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

/**
 * Special statements used for the init/cond/loop parts of a for loop.
 * Is like the comma operator in C and JavaScript but can actually only
 * be used in the head of a for loop.
 */
class Comma extends Stmt {
    /** @var Expr\Expr[] */
    private $exprs = [];

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

    /** @return \PhpParser\Node\Expr[] */
    public function unparseNodes():array {
        $nodes = [];
        foreach ($this->exprs as $expr) {
            $nodes[] = $expr->unparseExpr();
        }
        return $nodes;
    }

    public function split():array {
        return $this->exprs;
    }
}

class For_ extends ControlStructure {
    /** @var Comma */
    private $init;
    /** @var Comma */
    private $cond;
    /** @var Comma */
    private $loop;
    /** @var Block */
    private $body;

    public function __construct(CodeLoc $loc, Comma $init, Comma $cond, Comma $loop, Block $body) {
        parent::__construct($loc);
        $this->init = $init;
        $this->cond = $cond;
        $this->loop = $loop;
        $this->body = $body;
    }

    public function subStmts():array {
        return [
            $this->init,
            $this->cond,
            $this->loop,
            $this->body,
        ];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\For_(
            [
                'init'  => $this->init->unparseNodes(),
                'cond'  => $this->cond->unparseNodes(),
                'loop'  => $this->loop->unparseNodes(),
                'stmts' => $this->body->unparseNodes(),
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

class Switch_ extends ControlStructure {
    /** @var Expr\Expr */
    private $expr;
    /** @var Case_[] */
    private $cases;

    /**
     * @param CodeLoc   $loc
     * @param Expr\Expr $expr
     * @param Case_[]   $cases
     */
    public function __construct(CodeLoc $loc, Expr\Expr $expr, array $cases) {
        parent::__construct($loc);
        $this->expr  = $expr;
        $this->cases = $cases;
    }

    public function subStmts():array {
        $stmts = [$this->expr];
        foreach ($this->cases as $case) {
            foreach ($case->subStmts() as $stmt) {
                $stmts[] = $stmt;
            }
        }
        return $stmts;
    }

    public function unparseStmt() {
        $cases = [];
        foreach ($this->cases as $case) {
            $cases[] = $case->unparse();
        }
        return new \PhpParser\Node\Stmt\Switch_(
            $this->expr->unparseExpr(),
            $cases
        );
    }
}

class Case_ extends Node {
    /** @var Expr\Expr|null */
    private $expr;
    /** @var Block */
    private $stmt;

    /**
     * @param CodeLoc        $loc
     * @param Expr\Expr|null $expr
     * @param Block          $stmt
     */
    public function __construct(CodeLoc $loc, Expr\Expr $expr = null, Block $stmt) {
        parent::__construct($loc);
        $this->expr = $expr;
        $this->stmt = $stmt;
    }

    public function subStmts():array {
        $stmts = [$this->stmt];
        if ($this->expr) {
            $stmts[] = $this->expr;
        }
        return $stmts;
    }

    public function unparse():\PhpParser\Node\Stmt\Case_ {
        return new \PhpParser\Node\Stmt\Case_(
            $this->expr ? $this->expr->unparseExpr() : null,
            $this->stmt->unparseNodes()
        );
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

class While_ extends ControlStructure {
    /** @var Expr\Expr */
    private $cond;
    /** @var Block */
    private $body;

    /**
     * @param CodeLoc   $loc
     * @param Expr\Expr $cond
     * @param Block     $body
     */
    public function __construct(CodeLoc $loc, Expr\Expr $cond, Block $body) {
        parent::__construct($loc);
        $this->cond = $cond;
        $this->body = $body;
    }

    public function subStmts():array {
        return [$this->cond, $this->body];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\While_(
            $this->cond->unparseExpr(),
            $this->body->unparseNodes()
        );
    }
}

class Try_ extends ControlStructure {
    /** @var Block */
    private $body;
    /** @var Catch_[] */
    private $catches;
    /** @var Block */
    private $finally;

    /**
     * @param CodeLoc  $loc
     * @param Block    $body
     * @param Catch_[] $catches
     * @param Block    $finally
     */
    public function __construct(CodeLoc $loc, Block $body, array $catches, Block $finally) {
        parent::__construct($loc);
        $this->body    = $body;
        $this->catches = $catches;
        $this->finally = $finally;
    }

    public function subStmts():array {
        $stmts = [$this->body];
        foreach ($this->catches as $catch) {
            foreach ($catch->subStmts() as $stmt) {
                $stmts[] = $stmt;
            }
        }
        return $stmts;
    }

    public function unparseStmt() {
        $cathes = [];
        foreach ($this->catches as $catch) {
            $cathes[] = $catch->unparse();
        }
        return new \PhpParser\Node\Stmt\TryCatch(
            $this->body->unparseNodes(),
            $cathes,
            $this->finally->unparseNodes() ?: null
        );
    }
}

class Catch_ extends Node {
    /** @var string */
    private $class;
    /** @var string */
    private $variable;
    /** @var Block */
    private $body;

    /**
     * @param CodeLoc $loc
     * @param string  $class
     * @param string  $variable
     * @param Block   $body
     */
    public function __construct(CodeLoc $loc, string $class, string $variable, Block $body) {
        parent::__construct($loc);
        $this->class    = $class;
        $this->variable = $variable;
        $this->body     = $body;
    }

    public function subStmts():array {
        return [$this->body];
    }

    public function unparse():\PhpParser\Node\Stmt\Catch_ {
        return new \PhpParser\Node\Stmt\Catch_(
            new \PhpParser\Node\Name\FullyQualified($this->class),
            $this->variable,
            $this->body->unparseNodes()
        );
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

class Label_ extends SingleStmt {
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
        return new \PhpParser\Node\Stmt\Label($this->name);
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
