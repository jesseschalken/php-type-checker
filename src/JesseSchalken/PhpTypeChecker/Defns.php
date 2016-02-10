<?php

namespace JesseSchalken\PhpTypeChecker\Defns;

use JesseSchalken\PhpTypeChecker\HasCodeLoc;
use JesseSchalken\PhpTypeChecker\Decls;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Function_;
use JesseSchalken\PhpTypeChecker\Node;
use JesseSchalken\PhpTypeChecker\Stmt;
use JesseSchalken\PhpTypeChecker\Type;
use function JesseSchalken\PhpTypeChecker\extract_namespace;
use function JesseSchalken\PhpTypeChecker\remove_namespace;

interface HasNamespace {
    public function namespace_():string;
}

abstract class Definition extends Stmt\SingleStmt {
    public final function findDefinitions():array {
        $decls   = parent::findDefinitions();
        $decls[] = $this;
        return $decls;
    }

    public function localSubStmts():array {
        return [];
    }
}

abstract class GlobalDefinition extends Definition {
}

abstract class LocalDefinition extends Definition {
}

abstract class VariableType extends Definition {
    /** @var string */
    private $name;
    /** @var Type\Type */
    private $type;

    public function __construct(HasCodeLoc $loc, string $name, Type\Type $type) {
        parent::__construct($loc);
        $this->name = $name;
        $this->type = $type;
    }

    public function name():string {
        return $this->name;
    }

    public function type():Type\Type {
        return $this->type;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseStmt() {
        return null;
    }
}

class GlobalVariableType extends VariableType {
    public function gatherGlobalDecls(Decls\GlobalDecls $decls) {
        parent::gatherGlobalDecls($decls);
        $decls->addGlobal($this->name(), $this->type());
    }
}

class LocalVariableType extends VariableType {
    public function gatherLocalDecls(Decls\LocalDecls $decls) {
        parent::gatherLocalDecls($decls);
        $decls->addLocal($this->name(), $this->type());
    }
}

class Label_ extends LocalDefinition {
    /** @var string */
    private $name;

    public function __construct(HasCodeLoc $loc, string $name) {
        parent::__construct($loc);
        $this->name = $name;
    }

    public function subStmts():array {
        return [];
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Label($this->name);
    }

    public function gatherLocalDecls(Decls\LocalDecls $decls) {
        parent::gatherLocalDecls($decls);
        $decls->addLabel($this->name);
    }
}

class Const_ extends GlobalDefinition implements HasNamespace {
    /** @var string */
    private $name;
    /** @var Expr\Expr */
    private $value;

    /**
     * @param HasCodeLoc   $loc
     * @param string    $name
     * @param Expr\Expr $value
     */
    public function __construct(HasCodeLoc $loc, string $name, Expr\Expr $value) {
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

    public final function namespace_():string {
        return extract_namespace($this->name());
    }

    public function gatherGlobalDecls(Decls\GlobalDecls $decls) {
        parent::gatherGlobalDecls($decls);
        $decls->addConstant($this->name, $this->value);
    }
}

class FunctionDefinition extends GlobalDefinition implements HasNamespace {
    /** @var string */
    private $name;
    /** @var Stmt\Block */
    private $body;
    /** @var Function_\Function_ */
    private $type;

    public function __construct(HasCodeLoc $loc, string $name, Function_\Function_ $type, Stmt\Block $body) {
        parent::__construct($loc);
        $this->name = $name;
        $this->type = $type;
        $this->body = $body;
    }

    public function name():string {
        return $this->name;
    }

    public function subStmts():array {
        $stmts   = $this->type->subStmts();
        $stmts[] = $this->body;
        return $stmts;
    }

    public function unparseStmt() {
        return new \PhpParser\Node\Stmt\Function_(
            remove_namespace($this->name),
            array_replace(
                $this->type->unparseAttributes(),
                [
                    'stmts' => $this->body->unparseNodes(),
                ]
            )
        );
    }

    public final function namespace_():string {
        return extract_namespace($this->name());
    }

    public function gatherGlobalDecls(Decls\GlobalDecls $decls) {
        parent::gatherGlobalDecls($decls);
        $decls->addFunction($this->name, $this->type);
    }
}

abstract class Classish extends GlobalDefinition implements HasNamespace {
    private $name;

    public function __construct(HasCodeLoc $loc, string $name) {
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

    public final function namespace_():string {
        return extract_namespace($this->name());
    }
}

class Trait_ extends Classish {
    /** @var ClassMember[] */
    private $members = [];

    /**
     * @param HasCodeLoc       $loc
     * @param string        $name
     * @param ClassMember[] $members
     */
    public function __construct(HasCodeLoc $loc, string $name, array $members) {
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
     * @param HasCodeLoc       $loc
     * @param string        $name
     * @param ClassMember[] $members
     * @param string|null   $parent
     * @param string[]      $implements
     * @param bool          $abstract
     * @param bool          $final
     */
    public function __construct(
        HasCodeLoc $loc,
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
     * @param HasCodeLoc   $loc
     * @param string    $name
     * @param string[]  $extends
     * @param Method_[] $methods
     */
    public function __construct(HasCodeLoc $loc, string $name, array $extends, array $methods) {
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

abstract class AbstractClassMember extends Node {
    public abstract function unparse():\PhpParser\Node;

    /**
     * @return Stmt\Stmt[]
     */
    public abstract function subStmts():array;
}

class ClassConstant extends AbstractClassMember {
    /** @var string */
    private $name;
    /** @var Expr\Expr */
    private $value;

    public function __construct(HasCodeLoc $loc, string $name, Expr\Expr $value) {
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
     * @param HasCodeLoc  $loc
     * @param string[] $traits
     */
    public function __construct(HasCodeLoc $loc, array $traits) {
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

    public function __construct(HasCodeLoc $loc, string $trait, string $method, array $insteadOf) {
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
     * @param HasCodeLoc     $loc
     * @param string      $alias
     * @param string      $method
     * @param null|string $trait
     * @param null|string $visibility
     */
    public function __construct(HasCodeLoc $loc, string $alias, string $method, $trait, $visibility) {
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
     * @param HasCodeLoc $loc
     * @param string  $visibility
     * @param bool    $static
     */
    public function __construct(HasCodeLoc $loc, string $visibility, bool $static) {
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
     * @param HasCodeLoc        $loc
     * @param string         $name
     * @param Type\Type      $type
     * @param Expr\Expr|null $default
     * @param string         $visibility
     * @param bool           $static
     */
    public function __construct(
        HasCodeLoc $loc,
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

class Method_ extends ClassMember {
    /** @var string */
    private $name;
    /** @var Function_\Function_ */
    private $type;
    /** @var Stmt\Block|null */
    private $body;
    /** @var bool */
    private $final;

    /**
     * @param HasCodeLoc             $loc
     * @param string              $name
     * @param Function_\Function_ $type
     * @param Stmt\Block|null     $body
     * @param bool                $final
     * @param string              $visibility
     * @param bool                $static
     */
    public function __construct(
        HasCodeLoc $loc,
        string $name,
        Function_\Function_ $type,
        Stmt\Block $body = null,
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

