<?php

namespace JesseSchalken\PhpTypeChecker\Parser;

use JesseSchalken\MagicUtils\DeepClone;
use JesseSchalken\PhpTypeChecker\CodeLoc;
use JesseSchalken\PhpTypeChecker\LValue;
use JesseSchalken\PhpTypeChecker\Constants;
use JesseSchalken\PhpTypeChecker\ErrorReceiver;
use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Call;
use function JesseSchalken\PhpTypeChecker\node_sub_nodes;
use function JesseSchalken\PhpTypeChecker\normalize_constant;
use JesseSchalken\PhpTypeChecker\Stmt;
use JesseSchalken\PhpTypeChecker\Type;
use function JesseSchalken\MagicUtils\clone_ref;

abstract class DefinedNames {
    private $names = [];

    /**
     * @param string[] $names
     */
    public function __construct(array $names = []) {
        foreach ($names as $name) {
            $this->add($name);
        }
    }

    public final function has(string $name):bool {
        return isset($this->names[$this->normalize($name)]);
    }

    public final function add(string $name) {
        $this->names[$this->normalize($name)] = true;
    }

    abstract protected function normalize(string $name):string;

    public final function create(string $prefix):string {
        for ($i = 0; $name = $prefix . $i, $this->has($name); $i++) {
        }

        $this->add($name);
        return $name;
    }
}

abstract class NamespaceUses {
    /** @var \PhpParser\Node\Name */
    private $namespace;
    /** @var \PhpParser\Node\Name[] */
    private $uses = [];
    /** @var DefinedNames */
    private $defined;
    /** @var string[] */
    private $original = [];

    public function __construct(\PhpParser\Node\Name $namespace, DefinedNames $defined) {
        $this->namespace = $namespace;
        $this->defined   = $defined;
    }

    public function getMap():array {
        $map = [];
        foreach ($this->uses as $k => $use) {
            $map[$this->original[$k]] = $use->toString();
        }
        return $map;
    }

    public final function resolve(\PhpParser\Node\Name $name, NamespaceUses $classes):\PhpParser\Node\Name {
        if ($name->isFullyQualified()) {
            // \Foo\Bar
            return $name;
        } elseif ($name->isRelative()) {
            // namespace\Foo\Bar
            return \PhpParser\Node\Name::concat($this->namespace, $name);
        } elseif ($name->isQualified()) {
            // Foo\Bar
            return \PhpParser\Node\Name::concat($classes->resolveUnqualified($name->getFirst()), $name->slice(1));
        } else {
            // Bar
            return $this->resolveUnqualified($name->getFirst());
        }
    }

    private function resolveUnqualified(string $alias):\PhpParser\Node\Name {
        if (isset($this->uses[$this->normalize($alias)])) {
            return $this->uses[$this->normalize($alias)];
        } else {
            return $this->resolveDefault($alias);
        }
    }

    protected function resolveDefault(string $alias):\PhpParser\Node\Name {
        return \PhpParser\Node\Name::concat($this->namespace, $alias);
    }

    public final function add(\PhpParser\Node\Name $name, string $alias = null) {
        $alias      = $alias ?: $name->getLast();
        $normalized = $this->normalize($alias);

        $this->original[$normalized] = $alias;
        $this->uses[$normalized]     = $name;
    }

    protected final function defined(\PhpParser\Node\Name $name):bool {
        return $this->defined->has($name->toString());
    }

    protected function normalize(string $alias):string {
        return $alias;
    }
}

final class GlobalDefinedNames {
    use DeepClone;

    /** @var DefinedNames */
    public $classes;
    /** @var DefinedNames */
    public $constants;
    /** @var DefinedNames */
    public $functions;

    public function __construct() {
        $this->classes = new class extends DefinedNames {
            protected function normalize(string $name):string {
                return strtolower($name);
            }
        };

        $this->constants = new class extends DefinedNames {
            protected function normalize(string $name):string {
                return normalize_constant($name);
            }
        };

        $this->functions = new class extends DefinedNames {
            protected function normalize(string $name):string {
                return strtolower($name);
            }
        };
    }

    /**
     * @param \PhpParser\Node[] $nodes
     * @param string            $prefix
     */
    public function addNodes(array $nodes, string $prefix = '') {
        foreach ($nodes as $node) {
            $this->addNode($node, $prefix);
        }
    }

    /**
     * @param \PhpParser\Node $node
     * @param string          $prefix
     * @return void
     */
    public function addNode(\PhpParser\Node $node, string $prefix = '') {
        if ($node instanceof \PhpParser\Node\Stmt\Function_) {
            $this->functions->add($prefix . $node->name);
        } elseif ($node instanceof \PhpParser\Node\Stmt\ClassLike) {
            $this->classes->add($prefix . $node->name);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Const_) {
            foreach ($node->consts as $const) {
                $this->constants->add($prefix . $const->name);
            }
        } elseif ($node instanceof \PhpParser\Node\Stmt\Namespace_) {
            $prefix = $node->name ? $node->name->toString() . '\\' : '';
        }

        $this->addNodes(node_sub_nodes($node), $prefix);
    }
}

final class ParsedFile {
    /** @var string */
    public $path;
    /** @var string */
    public $contents = '';
    /** @var \PhpParser\Node[] */
    public $nodes = [];
    /** @var string */
    public $shebang = '';
    /** @var int */
    public $lineOffset = 0;

    public function __construct(string $path, string $contents, ErrorReceiver $errors) {
        $this->path     = $path;
        $this->contents = $contents;
        if (substr($this->contents, 0, 2) == "#!") {
            $pos              = strpos($this->contents, "\n") + 1;
            $this->shebang    = substr($this->contents, 0, $pos);
            $this->contents   = substr($this->contents, $pos);
            $this->lineOffset = 1;
        }

        $parser = new \PhpParser\Parser\Php7(
            new \PhpParser\Lexer([
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

        $this->nodes = $parser->parse($this->contents);

        foreach ($parser->getErrors() as $error) {
            $errors->add($error->getRawMessage(), $this->locateError($error));
        }
        $this->contents = $contents;
    }

    public function locateError(\PhpParser\Error $error):CodeLoc {
        $line = $this->lineOffset + $error->getStartLine();
        $col  = $this->offsetToColumn($error->hasColumnInfo() ? $error->getAttributes()['startFilePos'] : null);
        return new CodeLoc($this->path, $line, $col);
    }

    public function locateNode(\PhpParser\Node $node):CodeLoc {
        $line = $this->lineOffset + $node->getLine();
        $col  = $this->offsetToColumn($node->getAttribute('startFilePos'));
        return new CodeLoc($this->path, $line, $col);
    }

    public function offsetToColumn(int $offset = null):int {
        if ($offset === null) {
            return 1;
        } else {
            $code      = $this->contents;
            $lineStart = strrpos($code, "\n", $offset - strlen($code));
            $lineStart = $lineStart === false ? 0 : $lineStart + 1;

            return $offset - $lineStart + 1;
        }
    }

    public function nullLoc():CodeLoc {
        return new CodeLoc($this->path, 1, 1);
    }
}

final class NamespaceContext {
    /** @var \PhpParser\Node\Name */
    private $namespace;
    /** @var NamespaceUses */
    private $useFunction;
    /** @var NamespaceUses This is used for all four of classes, interfaces, traits and namespaces */
    private $useClass;
    /** @var NamespaceUses */
    private $useConstant;

    public function __construct(\PhpParser\Node\Name $name = null, GlobalDefinedNames $globals) {
        $namespace = $name ?: new \PhpParser\Node\Name([]);

        $this->namespace = $namespace;

        $this->useClass = new class ($namespace, $globals->classes) extends NamespaceUses {
            protected function normalize(string $alias):string {
                return strtolower($alias);
            }
        };

        $this->useFunction = new class ($namespace, $globals->functions) extends NamespaceUses {
            protected function normalize(string $alias):string {
                return strtolower($alias);
            }

            protected function resolveDefault(string $alias):\PhpParser\Node\Name {
                $local = parent::resolveDefault($alias);

                return $this->defined($local) ? $local : new \PhpParser\Node\Name([$alias]);
            }
        };

        $this->useConstant = new class ($namespace, $globals->constants) extends NamespaceUses {
            protected function resolveDefault(string $alias):\PhpParser\Node\Name {
                $local = parent::resolveDefault($alias);

                return $this->defined($local) ? $local : new \PhpParser\Node\Name([$alias]);
            }
        };
    }

    public function __clone() {
        clone_ref($this->useFunction);
        clone_ref($this->useClass);
        clone_ref($this->useConstant);
    }

    public function getNamespace():string {
        return $this->namespace->toString();
    }

    public function getClassAliasMap():array {
        return $this->useClass->getMap();
    }

    public function resolveFunction(\PhpParser\Node\Name $name):string {
        return $this->useFunction->resolve($name, $this->useClass)->toString();
    }

    public function resolveClass(\PhpParser\Node\Name $name):string {
        return $this->useClass->resolve($name, $this->useClass)->toString();
    }

    public function resolveConstant(\PhpParser\Node\Name $name):string {
        return $this->useConstant->resolve($name, $this->useClass)->toString();
    }

    public function addFunction(\PhpParser\Node\Name $name, string $alias = null) {
        $this->useFunction->add($name, $alias);
    }

    public function addClass(\PhpParser\Node\Name $name, string $alias = null) {
        $this->useClass->add($name, $alias);
    }

    public function addConstant(\PhpParser\Node\Name $name, string $alias = null) {
        $this->useConstant->add($name, $alias);
    }
}

final class Parser {
    /** @var NamespaceContext */
    private $namespace;
    /** @var GlobalDefinedNames */
    private $globals;
    /** @var ParsedFile */
    private $file;
    /** @var string|null */
    private $class;
    /** @var string|null */
    private $parent;
    /** @var string|null */
    private $function;
    /** @var ErrorReceiver */
    private $errors;

    public function __construct(ParsedFile $file, GlobalDefinedNames $globals, ErrorReceiver $errors) {
        $this->globals   = $globals;
        $this->file      = $file;
        $this->errors    = $errors;
        $this->namespace = new NamespaceContext(null, $globals);
    }

    private function withMagicConstants(
        string $class = null,
        string $parent = null,
        string $function = null
    ):self {
        $self = clone $this;

        $self->class    = $class;
        $self->parent   = $parent;
        $self->function = $function;

        return $self;
    }

    public function __clone() {
        clone_ref($this->namespace);
        clone_ref($this->class);
        clone_ref($this->parent);
        clone_ref($this->function);
    }

    private function resolveClass(\PhpParser\Node\Name $name):Expr\AbstractClassName {
        $loc = $this->locateNode($name);
        if (
            !$name->isFullyQualified() &&
            !$name->isRelative() &&
            !$name->isQualified()
        ) {
            switch (strtolower($name->getFirst())) {
                case 'self':
                    if ($this->class) {
                        return new Expr\ClassName($loc, $this->class);
                    } else {
                        throw new \Exception('Cannot use "self" outside a class');
                    }
                case 'parent':
                    if ($this->parent) {
                        return new Expr\ClassName($loc, $this->parent);
                    } else {
                        throw new \Exception('Cannot use "parent" without a parent class');
                    }
                case 'static':
                    return new Expr\StaticClassName($loc);
            }
        }

        return new Expr\ClassName($loc, $this->namespace->resolveClass($name));
    }

    private function resolveConst(\PhpParser\Node\Name $name):string {
        return $this->namespace->resolveConstant($name);
    }

    private function resolveFunction(\PhpParser\Node\Name $name):string {
        return $this->namespace->resolveFunction($name);
    }

    /**
     * @param CodeLoc                $loc
     * @param \PhpParser\Node\Stmt[] $nodes
     * @return Stmt\Block
     * @throws \Exception
     */
    public function parseStmts(CodeLoc $loc, array $nodes):Stmt\Block {
        $stmts = new Stmt\Block($loc);
        foreach ($nodes as $node) {
            $this->parseVariableDocBlocks($node, $stmts);

            foreach ($this->parseStmt($node)->split() as $stmt) {
                $stmts->add($stmt);
            }
        }
        return $stmts;
    }

    private function parseVariableDocBlocks(\PhpParser\Node $node, Stmt\Block $block) {
        foreach ($node->getAttribute('comments', []) as $comment) {
            if ($comment instanceof \PhpParser\Comment\Doc) {
                $docBlock = $this->parseDocBlock($comment);
                $codeLoc  = $this->locateComment($docBlock);
                foreach ($docBlock->getTags() as $tag) {
                    if ($tag instanceof \phpDocumentor\Reflection\DocBlock\Tag\VarTag) {
                        $name = substr($tag->getVariableName(), 1);
                        $type = $this->parseDocType($codeLoc, $tag->getType(), $docBlock->getContext())
                            ?: Type\Type::none($codeLoc);
                        switch ($tag->getName()) {
                            case 'var':
                                $block->add(new Stmt\LocalVariableType($codeLoc, $name, $type));
                                break;
                            case 'global':
                            case 'xglobal':
                                $block->add(new Stmt\GlobalVariableType($codeLoc, $name, $type));
                                break;
                        }
                    }
                }
            }
        }
    }

    private function parseStmt(\PhpParser\Node $node):Stmt\Stmt {
        $loc = $this->locateNode($node);
        if ($node instanceof \PhpParser\Node\Expr) {
            return $this->parseExpr($node);
        } elseif ($node instanceof \PhpParser\Node\Stmt\If_) {
            if ($node->else) {
                $false = $this->parseStmts($this->locateNode($node->else), $node->else->stmts);
            } else {
                $false = $this->parseStmts($loc, []);
            }

            foreach (array_reverse($node->elseifs) as $elseIf) {
                $loc1  = $this->locateNode($elseIf);
                $false = new Stmt\Block($loc1, [new Stmt\If_(
                    $loc1,
                    $this->parseExpr($elseIf->cond),
                    $this->parseStmts($loc1, $elseIf->stmts),
                    $false
                )]);
            }

            return new Stmt\If_(
                $loc,
                $this->parseExpr($node->cond),
                $this->parseStmts($loc, $node->stmts),
                $false
            );
        } elseif ($node instanceof \PhpParser\Node\Stmt\Return_) {
            return new Stmt\Return_($loc, $this->parseExprNull($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Stmt\Namespace_) {
            $self = clone $this;

            $self->namespace = new NamespaceContext($node->name, $this->globals);
            return $self->parseStmts($loc, $node->stmts);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Class_) {
            return $this->parseClass($node);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Function_) {
            $name = $this->prefixName($node->name);
            $self = $this->withMagicConstants(null, null, $name);
            return new Stmt\Function_(
                $loc,
                $name,
                $self->parseFunctionType($node),
                $self->parseStmts($loc, $node->stmts)
            );
        } elseif ($node instanceof \PhpParser\Node\Stmt\Interface_) {
            $name = $this->prefixName($node->name);
            $self = $this->withMagicConstants($name, null, null);

            $extends = [];
            foreach ($node->extends as $extend) {
                $extends[] = $self->resolveClass($extend)->toString();
            }
            return new Stmt\Interface_($loc, $name, $extends, $self->parseClassMembers($node));
        } elseif ($node instanceof \PhpParser\Node\Stmt\Trait_) {
            $name = $this->prefixName($node->name);
            $self = $this->withMagicConstants($name, null, null);
            return new Stmt\Trait_($loc, $name, $self->parseClassMembers($node));
        } elseif ($node instanceof \PhpParser\Node\Stmt\Use_) {
            $this->addUses($node->uses, $node->type);
            return new Stmt\Block($loc);
        } elseif ($node instanceof \PhpParser\Node\Stmt\GroupUse) {
            $this->addUses($node->uses, $node->type, $node->prefix);
            return new Stmt\Block($loc);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Foreach_) {
            return new Stmt\Foreach_(
                $loc,
                $this->parseExpr($node->expr),
                $this->parseExprNull($node->keyVar),
                $this->parseExpr($node->valueVar),
                $node->byRef,
                $this->parseStmts($loc, $node->stmts)
            );
        } elseif ($node instanceof \PhpParser\Node\Stmt\Echo_) {
            return new Stmt\Echo_($loc, $this->parseExprs($node->exprs));
        } elseif ($node instanceof \PhpParser\Node\Stmt\InlineHTML) {
            return new Stmt\InlineHTML($loc, $node->value);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Const_) {
            $stmts = [];
            foreach ($node->consts as $const) {
                $stmts[] = new Stmt\Const_(
                    $this->locateNode($const),
                    $this->prefixName($const->name),
                    $this->parseExpr($const->value)
                );
            }
            return new Stmt\Block($loc, $stmts);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Throw_) {
            return new Stmt\Throw_($loc, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Stmt\Static_) {
            $stmts = [];
            foreach ($node->vars as $finallyVar) {
                $stmts[] = new Stmt\StaticVar(
                    $this->locateNode($finallyVar),
                    $finallyVar->name,
                    $this->parseExprNull($finallyVar->default)
                );
            }
            return new Stmt\Block($loc, $stmts);
        } elseif ($node instanceof \PhpParser\Node\Stmt\For_) {
            return new Stmt\For_(
                $loc,
                new Stmt\Comma($loc, $this->parseExprs($node->init)),
                new Stmt\Comma($loc, $this->parseExprs($node->cond)),
                new Stmt\Comma($loc, $this->parseExprs($node->loop)),
                $this->parseStmts($loc, $node->stmts)
            );
        } elseif ($node instanceof \PhpParser\Node\Stmt\Break_) {
            if ($node->num === null) {
                $levels = 1;
            } elseif ($node->num instanceof \PhpParser\Node\Scalar\LNumber) {
                $levels = $node->num->value;
            } else {
                throw new \Exception('"break" statement must use a constant operand');
            }

            return new Stmt\Break_($loc, $levels);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Continue_) {
            if ($node->num === null) {
                $levels = 1;
            } elseif ($node->num instanceof \PhpParser\Node\Scalar\LNumber) {
                $levels = $node->num->value;
            } else {
                throw new \Exception('"continue" statement must use a constant operand');
            }

            return new Stmt\Continue_($loc, $levels);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Switch_) {
            $cases = [];
            foreach ($node->cases as $case) {
                $cases[] = new Stmt\Case_(
                    $this->locateNode($case),
                    $this->parseExprNull($case->cond),
                    $this->parseStmts($loc, $case->stmts)
                );
            }
            return new Stmt\Switch_(
                $loc,
                $this->parseExpr($node->cond),
                $cases
            );
        } elseif ($node instanceof \PhpParser\Node\Stmt\Unset_) {
            return new Stmt\Unset_($loc, $this->parseExprs($node->vars));
        } elseif ($node instanceof \PhpParser\Node\Stmt\While_) {
            return new Stmt\While_($loc, $this->parseExpr($node->cond), $this->parseStmts($loc, $node->stmts));
        } elseif ($node instanceof \PhpParser\Node\Stmt\TryCatch) {
            $body    = $this->parseStmts($loc, $node->stmts);
            $catches = [];
            $finally = $this->parseStmts($loc, $node->finallyStmts ?: []);

            foreach ($node->catches as $catch) {
                $catches[] = new Stmt\Catch_(
                    $this->locateNode($catch),
                    $this->resolveClass($catch->type)->toString(),
                    $catch->var,
                    $this->parseStmts($this->locateNode($catch), $catch->stmts)
                );
            }

            return new Stmt\Try_($loc, $body, $catches, $finally);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Do_) {
            return new Stmt\DoWhile(
                $loc,
                $this->parseStmts($loc, $node->stmts),
                $this->parseExpr($node->cond)
            );
        } elseif ($node instanceof \PhpParser\Node\Stmt\Global_) {
            $stmts = new Stmt\Block($loc);
            foreach ($node->vars as $var) {
                $stmts->add(new Stmt\Global_($this->locateNode($var), $this->parseExpr($var)));
            }
            return $stmts;
        } elseif ($node instanceof \PhpParser\Node\Stmt\Label) {
            return new Stmt\Label_($loc, $node->name);
        } elseif ($node instanceof \PhpParser\Node\Stmt\Goto_) {
            return new Stmt\Goto_($loc, $node->name);
        } else {
            throw new \Exception('Unhandled statement type: ' . get_class($node));
        }
    }

    private function prefixName(string $name):string {
        return \PhpParser\Node\Name::concat($this->namespace, $name)->toString();
    }

    /**
     * @param \PhpParser\Node\Stmt\ClassLike $node
     * @return Stmt\AbstractClassMember[]
     * @throws \Exception
     */
    private function parseClassMembers(\PhpParser\Node\Stmt\ClassLike $node):array {
        $stmts = [];
        foreach ($node->stmts as $stmt) {
            $loc = $this->locateNode($stmt);
            if ($stmt instanceof \PhpParser\Node\Stmt\ClassMethod) {
                $self    = $this->withMagicConstants(
                    $this->class,
                    $this->parent,
                    $stmt->name
                );
                $stmts[] = new Stmt\Method_(
                    $loc,
                    $stmt->name,
                    $self->parseFunctionType($stmt),
                    $stmt->stmts === null ? null : $self->parseStmts($loc, $stmt->stmts),
                    !!($stmt->type & \PhpParser\Node\Stmt\Class_::MODIFIER_FINAL),
                    $self->parseVisibility($stmt->type),
                    !!($stmt->type & \PhpParser\Node\Stmt\Class_::MODIFIER_STATIC)
                );
            } else if ($stmt instanceof \PhpParser\Node\Stmt\ClassConst) {
                foreach ($stmt->consts as $const) {
                    $stmts[] = new Stmt\ClassConstant(
                        $this->locateNode($stmt),
                        $const->name,
                        $this->parseExpr($const->value)
                    );
                }
            } else if ($stmt instanceof \PhpParser\Node\Stmt\Property) {
                $visibility = $this->parseVisibility($stmt->type);
                $static     = $stmt->type & \PhpParser\Node\Stmt\Class_::MODIFIER_STATIC;
                foreach ($stmt->props as $prop) {
                    $type = null;
                    if ($comment = $this->parseDocBlockOrNull($prop->getDocComment() ?: $stmt->getDocComment())) {
                        /** @var \phpDocumentor\Reflection\DocBlock\Tag\VarTag $tag */
                        foreach ($comment->getTagsByName('var') as $tag) {
                            $type = $type ?: $this->parseDocType(
                                $this->locateComment($comment),
                                $tag->getType(),
                                $comment->getContext()
                            );
                        }
                    }
                    $stmts[] = new Stmt\Property(
                        $this->locateNode($prop),
                        $prop->name,
                        $type ?: new Type\Mixed($loc),
                        $this->parseExprNull($prop->default),
                        $visibility,
                        $static
                    );
                }
            } else if ($stmt instanceof \PhpParser\Node\Stmt\TraitUse) {
                $traits = [];
                foreach ($stmt->traits as $trait) {
                    $traits[] = $this->resolveClass($trait)->toString();
                }
                $useTrait = new Stmt\UseTrait($loc, $traits);
                foreach ($stmt->adaptations as $adaption) {
                    if ($adaption instanceof \PhpParser\Node\Stmt\TraitUseAdaptation\Precedence) {
                        $insteadOf = [];
                        foreach ($adaption->insteadof as $name) {
                            $insteadOf[] = $this->resolveClass($name)->toString();
                        }
                        $useTrait->addInsteadOf(
                            new Stmt\UseTraitInsteadof(
                                $this->locateNode($adaption),
                                $this->resolveClass($adaption->trait)->toString(),
                                $adaption->method,
                                $insteadOf
                            )
                        );
                    } elseif ($adaption instanceof \PhpParser\Node\Stmt\TraitUseAdaptation\Alias) {
                        $useTrait->addAlias(
                            new Stmt\UseTraitAlias(
                                $this->locateNode($adaption),
                                $adaption->newName !== null ? $adaption->newName : $adaption->method,
                                $adaption->method,
                                $adaption->trait ? $this->resolveClass($adaption->trait)->toString() : null,
                                $adaption->newModifier !== null
                                    ? $this->parseVisibility($adaption->newModifier)
                                    : null
                            )
                        );
                    } else {
                        throw new \Exception('Unhandled type of trait adaption: ' . get_class($adaption));
                    }
                }
                $stmts[] = $useTrait;
            } else {
                throw new \Exception('Unhandled class member type: ' . get_class($stmt));
            }
        }
        return $stmts;
    }

    private function parseVisibility(int $type):string {
        if ($type & \PhpParser\Node\Stmt\Class_::MODIFIER_PUBLIC) {
            return Stmt\Visibility::PUBLIC;
        } else if ($type & \PhpParser\Node\Stmt\Class_::MODIFIER_PROTECTED) {
            return Stmt\Visibility::PROTECTED;
        } else if ($type & \PhpParser\Node\Stmt\Class_::MODIFIER_PRIVATE) {
            return Stmt\Visibility::PRIVATE;
        } else {
            return Stmt\Visibility::PUBLIC;
        }
    }

    /**
     * @param \PhpParser\Comment\Doc|null $comment
     * @return null|\phpDocumentor\Reflection\DocBlock
     */
    private function parseDocBlockOrNull(\PhpParser\Comment\Doc $comment = null) {
        return $comment ? $this->parseDocBlock($comment) : null;
    }

    private function parseDocBlock(\PhpParser\Comment\Doc $comment):\phpDocumentor\Reflection\DocBlock {
        return new \phpDocumentor\Reflection\DocBlock(
            $comment->getText(),
            $this->getDocBlockContext(),
            new \phpDocumentor\Reflection\DocBlock\Location($comment->getLine(), 1)
        );
    }

    private function getDocBlockContext():\phpDocumentor\Reflection\DocBlock\Context {
        return new \phpDocumentor\Reflection\DocBlock\Context(
            $this->namespace->getNamespace(),
            $this->namespace->getClassAliasMap()
        );
    }

    private function locateComment(\phpDocumentor\Reflection\DocBlock $docBlock):CodeLoc {
        return new CodeLoc(
            $this->file->path,
            $docBlock->getLocation()->getLineNumber(),
            $docBlock->getLocation()->getColumnNumber()
        );
    }

    /**
     * @param CodeLoc                                    $loc
     * @param string                                     $string
     * @param \phpDocumentor\Reflection\DocBlock\Context $context
     * @return Type\Type|null
     * @throws \Exception
     */
    private function parseDocType(
        CodeLoc $loc,
        string $string,
        \phpDocumentor\Reflection\DocBlock\Context $context
    ) {
        $context = new \phpDocumentor\Reflection\Types\Context(
            $context->getNamespace(),
            $context->getNamespaceAliases()
        );
        try {
            $type = (new \phpDocumentor\Reflection\TypeResolver())->resolve($string, $context);
        } catch (\InvalidArgumentException $e) {
            return null;
        }
        return !$type ? null : $this->parseDocTypeObject($loc, $type, $context);
    }

    private function parseDocTypeObject(
        CodeLoc $loc,
        \phpDocumentor\Reflection\Type $type,
        \phpDocumentor\Reflection\Types\Context $context
    ):Type\Type {
        if ($type instanceof \phpDocumentor\Reflection\Types\Object_) {
            $fqsen = $type->getFqsen();
            if ($fqsen) {
                return new Type\Class_($loc, trim($fqsen->__toString(), '\\'));
            } else {
                return new Type\Object($loc);
            }
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Array_) {
            $value = $type->getValueType();
            if ($value instanceof \phpDocumentor\Reflection\Types\Mixed) {
                return new Type\Object($loc);
            } else {
                return new Type\Array_($loc, $this->parseDocTypeObject($loc, $value, $context));
            }
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Null_) {
            return new Type\SingleValue($loc, null);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Void) {
            return new Type\SingleValue($loc, null);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Static_) {
            return new Type\TypeVar($loc, Type\TypeVar::STATIC, $this->selfType($loc));
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Resource) {
            return new Type\Resource($loc);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Mixed) {
            return new Type\Mixed($loc);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Scalar) {
            return Type\Type::scalar($loc);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\This) {
            return new Type\TypeVar($loc, Type\TypeVar::THIS, $this->selfType($loc));
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Boolean) {
            return Type\Type::bool($loc);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Callable_) {
            return new Type\Callable_($loc);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Compound) {
            $union = new Type\Union($loc);
            for ($i = 0; $type->has($i); $i++) {
                foreach ($this->parseDocTypeObject($loc, $type->get($i), $context) as $t) {
                    $union = $union->addType($t, new Type\DummyTypeContext());
                }
            }
            return $union;
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Self_) {
            return $this->selfType($loc);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Float_) {
            return new Type\Float_($loc);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\String_) {
            return new Type\String_($loc);
        } else if ($type instanceof \phpDocumentor\Reflection\Types\Integer) {
            return new Type\Int_($loc);
        } else {
            throw new \Exception('Unhandled PhpDoc type: ' . get_class($type));
        }
    }

    private function checkCompatible(Type\Type $sup, Type\Type $sub) {
        if (!$sup->containsType($sub, new Type\DummyTypeContext())) {
            $this->errors->add("Warning $sub is not compatible with $sup", $sup->loc());
        }
    }

    private function parseFunctionType(\PhpParser\Node\FunctionLike $node):Stmt\FunctionSignature {
        $loc = $this->locateNode($node);
        /**
         * @var Type\Type[] $paramTypes
         * @var Expr\Expr[] $paramDefaults
         */

        // First, get all the types from the type hints
        $paramTypes    = [];
        $paramDefaults = [];
        foreach ($node->getParams() as $param) {
            $name    = $param->name;
            $type    = $this->parseType($this->locateNode($param), $param->type);
            $default = $this->parseExprNull($param->default);

            if ($default && $default instanceof Constants\Literal && $default->value() === null) {
                $type = $type->addSingleType(new Type\SingleValue($default->loc(), null), new Type\DummyTypeContext());
            }

            $paramDefaults[$name] = $default;
            $paramTypes[$name]    = $type;
        }
        $returnType = $this->parseType($this->locateNode($node), $node->getReturnType());

        // If we have a doc comment, overwrite the types from the type hints with those
        // from the doc comment, but make sure they are trivially compatible.
        if ($comment = $this->parseDocBlockOrNull($node->getDocComment())) {
            /** @var \phpDocumentor\Reflection\DocBlock\Tag\ReturnTag $tag */
            foreach ($comment->getTagsByName('return') as $tag) {
                if ($type = $this->parseDocType(
                    $this->locateComment($comment),
                    $tag->getType(),
                    $comment->getContext()
                )
                ) {
                    $this->checkCompatible($returnType, $type);
                    $returnType = $type;
                }
            }
            /** @var \phpDocumentor\Reflection\DocBlock\Tag\ParamTag $tag */
            foreach ($comment->getTagsByName('param') as $tag) {
                $name = substr($tag->getVariableName(), 1);
                if ($type = $this->parseDocType(
                    $this->locateComment($comment),
                    $tag->getType(),
                    $comment->getContext()
                )
                ) {
                    if (!isset($paramTypes[$name])) {
                        $paramTypes[$name] = new Type\Mixed($this->locateComment($comment));
                    }
                    $this->checkCompatible($paramTypes[$name], $type);
                    $paramTypes[$name] = $type;
                }
            }
        }

        $params = [];
        foreach ($node->getParams() as $param) {
            $params[] = new Stmt\FunctionParam(
                $this->locateNode($param),
                $param->name,
                $paramDefaults[$param->name],
                $param->byRef,
                $param->variadic,
                $paramTypes[$param->name]
            );
        }
        return new Stmt\FunctionSignature($loc, $node->returnsByRef(), $params, $returnType);
    }

    /**
     * @param CodeLoc                          $loc
     * @param null|string|\PhpParser\Node\Name $type
     * @return Type\Type
     * @throws \Exception
     */
    private function parseType(CodeLoc $loc, $type):Type\Type {
        if ($type === null) {
            return new Type\Mixed($loc);
        } else if (is_string($type)) {
            switch (strtolower($type)) {
                case 'int':
                case 'integer':
                    return new Type\Int_($loc);
                    break;
                case 'string':
                    return new Type\String_($loc);
                    break;
                case 'double':
                case 'float':
                    return new Type\Float_($loc);
                    break;
                case 'bool':
                case 'boolean':
                    return Type\Type::bool($loc);
                    break;
                case 'null':
                case 'void':
                    return new Type\SingleValue($loc, null);
                    break;
                case 'object':
                    return new Type\Object($loc, null);
                    break;
                case 'resource':
                    return new Type\Resource($loc, null);
                    break;
                case 'array':
                    return new Type\Array_($loc, new Type\Mixed($loc));
                    break;
                case 'callable':
                    return new Type\Callable_($loc);
                default:
                    throw new \Exception('Invalid simple type: ' . $type);
            }
        } else if ($type instanceof \PhpParser\Node\Name) {
            return $this->resolveClass($type)->toType($this->class, false);
        } else {
            throw new \Exception('huh?');
        }
    }

    /**
     * @param \PhpParser\Node\Stmt\UseUse[] $uses
     * @param int                           $type_
     * @param \PhpParser\Node\Name|null     $prefix
     * @throws \Exception
     */
    private function addUses(array $uses, int $type_, \PhpParser\Node\Name $prefix = null) {
        foreach ($uses as $use) {
            $name = $prefix ? \PhpParser\Node\Name::concat($prefix, $use->name) : $use->name;
            $type = $use->type === \PhpParser\Node\Stmt\Use_::TYPE_UNKNOWN ? $type_ : $use->type;
            switch ($type) {
                case \PhpParser\Node\Stmt\Use_::TYPE_CONSTANT:
                    $this->namespace->addConstant($name, $use->alias);
                    break;
                case \PhpParser\Node\Stmt\Use_::TYPE_FUNCTION:
                    $this->namespace->addFunction($name, $use->alias);
                    break;
                case \PhpParser\Node\Stmt\Use_::TYPE_NORMAL:
                    $this->namespace->addClass($name, $use->alias);
                    break;
                default:
                    throw new \Exception('Invalid use type: ' . $type);
            }
        }
    }

    /**
     * @param \PhpParser\Node\Expr[] $nodes
     * @return Expr\Expr[]
     */
    private function parseExprs(array $nodes):array {
        $exprs = [];
        foreach ($nodes as $k => $v) {
            $exprs[] = $this->parseExpr($v);
        }
        return $exprs;
    }

    /**
     * @param \PhpParser\Node\Expr|null $node
     * @return Expr\Expr|null
     */
    private function parseExprNull(\PhpParser\Node\Expr $node = null) {
        return $node ? $this->parseExpr($node) : null;
    }

    /**
     * @param \PhpParser\Node\Expr|string $node
     * @param CodeLoc                     $loc
     * @return Expr\Expr
     * @throws \Exception
     */
    private function parseExprString($node, CodeLoc $loc):Expr\Expr {
        return is_string($node) ? new Constants\Literal($loc, $node) : $this->parseExpr($node);
    }

    /**
     * @param \PhpParser\Node\Expr|\PhpParser\Node\Name $node
     * @return Expr\Expr
     * @throws \Exception
     */
    private function parseExprClass($node):Expr\Expr {
        return $node instanceof \PhpParser\Node\Name
            ? $this->resolveClass($node)
            : $this->parseExpr($node);
    }

    private function parseExpr(\PhpParser\Node\Expr $node):Expr\Expr {
        $loc = $this->locateNode($node);
        if ($node instanceof \PhpParser\Node\Expr\Variable) {
            return new LValue\Variable($loc, $this->parseExprString($node->name, $loc));
        } elseif ($node instanceof \PhpParser\Node\Expr\ConstFetch) {
            $className = $this->resolveConst($node->name);
            switch (strtolower($className)) {
                case 'true':
                    return new Constants\Literal($loc, true);
                case 'false':
                    return new Constants\Literal($loc, false);
                case 'null':
                    return new Constants\Literal($loc, null);
                default:
                    return new Constants\GetConstant($loc, $className);
            }
        } elseif ($node instanceof \PhpParser\Node\Expr\Assign) {
            return new Expr\BinOp(
                $loc,
                $this->parseExpr($node->var),
                Expr\BinOp::ASSIGN,
                $this->parseExpr($node->expr)
            );
        } elseif ($node instanceof \PhpParser\Node\Scalar\LNumber) {
            return new Constants\Literal($loc, $node->value);
        } elseif ($node instanceof \PhpParser\Node\Scalar\DNumber) {
            return new Constants\Literal($loc, $node->value);
        } elseif ($node instanceof \PhpParser\Node\Expr\Include_) {
            switch ($node->type) {
                case \PhpParser\Node\Expr\Include_::TYPE_INCLUDE:
                    $require = false;
                    $once    = false;
                    break;
                case \PhpParser\Node\Expr\Include_::TYPE_INCLUDE_ONCE:
                    $require = false;
                    $once    = true;
                    break;
                case \PhpParser\Node\Expr\Include_::TYPE_REQUIRE:
                    $require = true;
                    $once    = false;
                    break;
                case \PhpParser\Node\Expr\Include_::TYPE_REQUIRE_ONCE:
                    $require = true;
                    $once    = true;
                    break;
                default:
                    throw new \Exception("Unknown require type: {$node->type}");
            }

            return new Expr\Include_($loc, $this->parseExpr($node->expr), $require, $once);
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Concat) {
            return new Expr\BinOp(
                $loc,
                $this->parseExpr($node->left),
                Expr\BinOp::CONCAT,
                $this->parseExpr($node->right)
            );
        } elseif ($node instanceof \PhpParser\Node\Scalar\MagicConst) {
            $type = new Constants\MagicConst($node->getName());
            $line = $node->getAttribute('startLine');
            return new Constants\GetMagicConst($loc, $type, $this->getMagicConstValue($type, $line));
        } elseif ($node instanceof \PhpParser\Node\Scalar\String_) {
            return new Constants\Literal($loc, $node->value);
        } elseif ($node instanceof \PhpParser\Node\Expr\StaticCall) {
            return new Call\StaticMethodCall(
                $loc,
                $this->parseArgs($node->args),
                $this->parseExprClass($node->class),
                $this->parseExprString($node->name, $loc)
            );
        } elseif ($node instanceof \PhpParser\Node\Expr\FuncCall) {
            $function = $node->name;
            $function = $function instanceof \PhpParser\Node\Name
                ? new Constants\Literal($this->locateNode($function), $this->resolveFunction($function))
                : $this->parseExpr($function);

            return new Call\FunctionCall($loc, $function, $this->parseArgs($node->args));
        } elseif ($node instanceof \PhpParser\Node\Expr\Array_) {
            $items = [];
            foreach ($node->items as $item) {
                $items[] = new Expr\ArrayItem(
                    $this->locateNode($item),
                    $this->parseExprNull($item->key),
                    $this->parseExpr($item->value),
                    $item->byRef
                );
            }
            return new Expr\Array_($loc, $items);
        } elseif ($node instanceof \PhpParser\Node\Expr\Empty_) {
            return new Expr\UnOp($loc, Expr\UnOp::EMPTY, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\ArrayDimFetch) {
            return new LValue\ArrayAccess(
                $loc,
                $this->parseExpr($node->var),
                $this->parseExprNull($node->dim)
            );
        } elseif ($node instanceof \PhpParser\Node\Expr\MethodCall) {
            return new Call\MethodCall(
                $loc,
                $this->parseArgs($node->args),
                $this->parseExpr($node->var),
                $this->parseExprString($node->name, $loc)
            );
        } elseif ($node instanceof \PhpParser\Node\Expr\New_) {
            $class = $node->class;
            $args  = $this->parseArgs($node->args);
            if ($class instanceof \PhpParser\Node\Stmt\Class_) {
                $classDfn = $this->parseClass($class, $this->globals->classes->create($this->prefixName('class')));
                return new Call\AnonymousNew($loc, $classDfn, $args);
            } else {
                $className = $this->parseExprClass($class);
                return new Call\New_($loc, $className, $args);
            }
        } elseif ($node instanceof \PhpParser\Node\Expr\BooleanNot) {
            return new Expr\UnOp($loc, Expr\UnOp::BOOL_NOT, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\Print_) {
            return new Expr\UnOp($loc, Expr\UnOp::PRINT, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\Closure) {
            $uses = [];
            foreach ($node->uses as $use) {
                $uses[] = new Expr\ClosureUse($this->locateNode($use), $use->var, $use->byRef);
            }
            return new Expr\Closure(
                $loc,
                $node->static,
                $this->parseFunctionType($node),
                $uses,
                $this->parseStmts($loc, $node->stmts)
            );
        } elseif ($node instanceof \PhpParser\Node\Expr\Ternary) {
            return new Expr\Ternary(
                $loc,
                $this->parseExpr($node->cond),
                $this->parseExprNull($node->if),
                $this->parseExpr($node->else)
            );
        } elseif ($node instanceof \PhpParser\Node\Scalar\EncapsedStringPart) {
            return new Constants\Literal($loc, $node->value);
        } elseif ($node instanceof \PhpParser\Node\Scalar\Encapsed) {
            $exprs = [];
            foreach ($node->parts as $part) {
                $exprs[] = $this->parseExpr($part);
            }
            return new Expr\ConcatMany($loc, $exprs);
        } elseif ($node instanceof \PhpParser\Node\Expr\StaticPropertyFetch) {
            $class = $this->parseExprClass($node->class);
            $prop  = $this->parseExprString($node->name, $loc);
            return new LValue\StaticProperty($loc, $class, $prop);
        } elseif ($node instanceof \PhpParser\Node\Expr\Isset_) {
            return new Expr\Isset_($loc, $this->parseExprs($node->vars));
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp) {
            return $this->parseBinaryOp($node, $loc);
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp) {
            return $this->parseAssignOp($node, $loc);
        } elseif ($node instanceof \PhpParser\Node\Expr\ErrorSuppress) {
            return new Expr\UnOp($loc, Expr\UnOp::SUPPRESS, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\PropertyFetch) {
            return new LValue\Property(
                $loc,
                $this->parseExpr($node->var),
                $this->parseExprString($node->name, $loc)
            );
        } elseif ($node instanceof \PhpParser\Node\Expr\Exit_) {
            return new Expr\Exit_($loc, $this->parseExprNull($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\Eval_) {
            return new Expr\UnOp($loc, Expr\UnOp::EVAL, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\Cast) {
            return $this->parseCast($node, $loc);
        } elseif ($node instanceof \PhpParser\Node\Expr\Instanceof_) {
            return new Expr\BinOp(
                $loc,
                $this->parseExpr($node->expr),
                Expr\BinOp:: INSTANCEOF,
                $this->parseExprClass($node->class)
            );
        } elseif ($node instanceof \PhpParser\Node\Expr\Clone_) {
            return new Expr\UnOp($loc, Expr\UnOp::CLONE, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\Yield_) {
            return new Expr\Yield_(
                $loc,
                $this->parseExprNull($node->key),
                $this->parseExprNull($node->value)
            );
        } elseif ($node instanceof \PhpParser\Node\Expr\ClassConstFetch) {
            if ($node->name === 'class') {
                if ($node->class instanceof \PhpParser\Node\Name) {
                    return $this->resolveClass($node->class);
                } else {
                    throw new \Exception('Use of ::class with expression is not supported');
                }
            } else {
                return new Constants\GetClassConstant($loc, $this->parseExprClass($node->class), $node->name);
            }
        } elseif ($node instanceof \PhpParser\Node\Expr\UnaryMinus) {
            return new Expr\UnOp($loc, Expr\UnOp::NEGATE, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\UnaryPlus) {
            return new Expr\UnOp($loc, Expr\UnOp::PLUS, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\PostInc) {
            return new Expr\UnOp($loc, Expr\UnOp::POST_INC, $this->parseExpr($node->var));
        } elseif ($node instanceof \PhpParser\Node\Expr\PreInc) {
            return new Expr\UnOp($loc, Expr\UnOp::PRE_INC, $this->parseExpr($node->var));
        } elseif ($node instanceof \PhpParser\Node\Expr\PostDec) {
            return new Expr\UnOp($loc, Expr\UnOp::POST_DEC, $this->parseExpr($node->var));
        } elseif ($node instanceof \PhpParser\Node\Expr\PreDec) {
            return new Expr\UnOp($loc, Expr\UnOp::PRE_DEC, $this->parseExpr($node->var));
        } elseif ($node instanceof \PhpParser\Node\Expr\List_) {
            $exprs = [];
            foreach ($node->vars as $v) {
                $exprs[] = $this->parseExprNull($v);
            }
            return new LValue\List_($loc, $exprs);
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignRef) {
            return new Expr\BinOp(
                $loc,
                $this->parseExpr($node->var),
                Expr\BinOp::ASSIGN_REF,
                $this->parseExpr($node->expr)
            );
        } elseif ($node instanceof \PhpParser\Node\Expr\BitwiseNot) {
            return new Expr\UnOp($loc, Expr\UnOp::BIT_NOT, $this->parseExpr($node->expr));
        } elseif ($node instanceof \PhpParser\Node\Expr\ShellExec) {
            $exprs = [];
            foreach ($node->parts as $part) {
                $exprs[] = $this->parseExpr($part);
            }
            return new Expr\ShellExec($loc, $exprs);
        } else {
            throw new \Exception('Unhandled expression type: ' . get_class($node));
        }
    }

    private function parseCast(\PhpParser\Node\Expr\Cast $node, CodeLoc $loc):Expr\Expr {
        if ($node instanceof \PhpParser\Node\Expr\Cast\Array_) {
            $type = Expr\Cast::ARRAY;
        } elseif ($node instanceof \PhpParser\Node\Expr\Cast\Bool_) {
            $type = Expr\Cast::BOOL;
        } elseif ($node instanceof \PhpParser\Node\Expr\Cast\Double) {
            $type = Expr\Cast::FLOAT;
        } elseif ($node instanceof \PhpParser\Node\Expr\Cast\Int_) {
            $type = Expr\Cast::INT;
        } elseif ($node instanceof \PhpParser\Node\Expr\Cast\Object_) {
            $type = Expr\Cast::OBJECT;
        } elseif ($node instanceof \PhpParser\Node\Expr\Cast\String_) {
            $type = Expr\Cast::STRING;
        } elseif ($node instanceof \PhpParser\Node\Expr\Cast\Unset_) {
            $type = Expr\Cast::UNSET;
        } else {
            throw new \Exception('Unknown cast type: ' . get_class($node));
        }

        return new Expr\Cast($loc, $type, $this->parseExpr($node->expr));
    }

    private function parseAssignOp(\PhpParser\Node\Expr\AssignOp $node, CodeLoc $loc):Expr\Expr {
        if ($node instanceof \PhpParser\Node\Expr\AssignOp\BitwiseAnd) {
            $type = Expr\BinOp::ASSIGN_BIT_AND;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\BitwiseOr) {
            $type = Expr\BinOp::ASSIGN_BIT_OR;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\BitwiseXor) {
            $type = Expr\BinOp::ASSIGN_BIT_XOR;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\Concat) {
            $type = Expr\BinOp::ASSIGN_CONCAT;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\Div) {
            $type = Expr\BinOp::ASSIGN_DIVIDE;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\Minus) {
            $type = Expr\BinOp::ASSIGN_SUBTRACT;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\Mod) {
            $type = Expr\BinOp::ASSIGN_MODULUS;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\Mul) {
            $type = Expr\BinOp::ASSIGN_MULTIPLY;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\Plus) {
            $type = Expr\BinOp::ASSIGN_ADD;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\Pow) {
            $type = Expr\BinOp::ASSIGN_EXPONENT;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\ShiftLeft) {
            $type = Expr\BinOp::ASSIGN_SHIFT_LEFT;
        } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp\ShiftRight) {
            $type = Expr\BinOp::ASSIGN_SHIFT_RIGHT;
        } else {
            throw new \Exception('Unhandled assignment operator: ' . get_class($node));
        }

        $left  = $this->parseExpr($node->var);
        $right = $this->parseExpr($node->expr);
        return new Expr\BinOp($loc, $left, $type, $right);
    }

    private function parseBinaryOp(\PhpParser\Node\Expr\BinaryOp $node, CodeLoc $loc):Expr\Expr {
        if ($node instanceof \PhpParser\Node\Expr\BinaryOp\BitwiseAnd) {
            $type = Expr\BinOp::BIT_AND;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\BitwiseOr) {
            $type = Expr\BinOp::BIT_OR;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\BitwiseXor) {
            $type = Expr\BinOp::BIT_XOR;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\BooleanAnd) {
            $type = Expr\BinOp::BOOl_AND;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\BooleanOr) {
            $type = Expr\BinOp::BOOl_OR;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Coalesce) {
            $type = Expr\BinOp::COALESCE;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Concat) {
            $type = Expr\BinOp::CONCAT;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Div) {
            $type = Expr\BinOp::DIVIDE;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Equal) {
            $type = Expr\BinOp::EQUAL;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Greater) {
            $type = Expr\BinOp::GREATER;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\GreaterOrEqual) {
            $type = Expr\BinOp::GREATER_OR_EQUAL;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Identical) {
            $type = Expr\BinOp::IDENTICAL;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\LogicalAnd) {
            $type = Expr\BinOp::LOGIC_AND;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\LogicalOr) {
            $type = Expr\BinOp::LOGIC_OR;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\LogicalXor) {
            $type = Expr\BinOp::LOGIC_XOR;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Minus) {
            $type = Expr\BinOp::SUBTRACT;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Mod) {
            $type = Expr\BinOp::MODULUS;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Mul) {
            $type = Expr\BinOp::MULTIPLY;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\NotEqual) {
            $type = Expr\BinOp::NOT_EQUAL;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\NotIdentical) {
            $type = Expr\BinOp::NOT_IDENTICAL;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Plus) {
            $type = Expr\BinOp::ADD;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Pow) {
            $type = Expr\BinOp::EXPONENT;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\ShiftLeft) {
            $type = Expr\BinOp::SHIFT_LEFT;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\ShiftRight) {
            $type = Expr\BinOp::SHIFT_RIGHT;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Smaller) {
            $type = Expr\BinOp::LESS;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\SmallerOrEqual) {
            $type = Expr\BinOp::LESS_OR_EQUAL;
        } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Spaceship) {
            $type = Expr\BinOp::SPACESHIP;
        } else {
            throw new \Exception('Unhandled binary operator: ' . get_class($node));
        }

        $left  = $this->parseExpr($node->left);
        $right = $this->parseExpr($node->right);
        return new Expr\BinOp($loc, $left, $type, $right);
    }

    private function getMagicConstValue(Constants\MagicConst $type, int $line):string {
        switch ($type->value()) {
            case Constants\MagicConst::LINE:
                return $line;
            case Constants\MagicConst::FILE:
                return realpath($this->file->path);
            case Constants\MagicConst::DIR:
                return dirname(realpath($this->file->path));
            case Constants\MagicConst::FUNCTION:
                return (string)$this->function;
            case Constants\MagicConst::CLASS_:
                return (string)$this->class;
            case Constants\MagicConst::TRAIT:
                return (string)$this->class;
            case Constants\MagicConst::METHOD:
                return $this->class ? "$this->class::$this->function" : "$this->function";
            case Constants\MagicConst::NAMESPACE:
                return $this->namespace->getNamespace();
            default:
                throw new \Exception("Invalid magic constant type: $type");
        }
    }

    /**
     * @param \PhpParser\Node\Arg[] $args
     * @return Call\CallArg[]
     * @throws \Exception
     */
    private function parseArgs(array $args):array {
        $result = [];
        foreach ($args as $arg) {
            $result[] = new Call\CallArg(
                $this->locateNode($arg),
                $this->parseExpr($arg->value),
                $arg->byRef,
                $arg->unpack
            );
        }
        return $result;
    }

    private function parseClass(\PhpParser\Node\Stmt\Class_ $node, string $name = null):Stmt\Class_ {
        $loc  = $this->locateNode($node);
        $name = $name ?: $this->prefixName($node->name);

        $parent     = $node->extends ? $this->resolveClass($node->extends)->toString() : null;
        $implements = [];
        foreach ($node->implements as $impl) {
            $implements[] = $this->resolveClass($impl)->toString();
        }

        return new Stmt\Class_(
            $loc,
            $name,
            $this->withMagicConstants($name, $parent, null)->parseClassMembers($node),
            $parent,
            $implements,
            $node->type & \PhpParser\Node\Stmt\Class_::MODIFIER_ABSTRACT ? true : false,
            $node->type & \PhpParser\Node\Stmt\Class_::MODIFIER_FINAL ? true : false
        );
    }

    private function locateNode(\PhpParser\Node $node):CodeLoc {
        return $this->file->locateNode($node);
    }

    private function selfType(CodeLoc $loc):Type\Class_ {
        if (!$this->class) {
            throw new \Exception('Use of "self" outside a class');
        } else {
            return new Type\Class_($loc, $this->class);
        }
    }
}

