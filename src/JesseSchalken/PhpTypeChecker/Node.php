<?php

namespace JesseSchalken\PhpTypeChecker\Node {

    use JesseSchalken\PhpTypeChecker\Node\Expr;
    use JesseSchalken\PhpTypeChecker\Node\Stmt;
    use JesseSchalken\PhpTypeChecker\Node\Type;
    use JesseSchalken\PhpTypeChecker\Parser;

    class CodeLoc {
        /** @var string */
        private $path;
        /** @var int */
        private $line;
        /** @var int */
        private $column;

        /**
         * @param string $path
         * @param int    $line
         * @param int    $column
         */
        public function __construct($path, $line, $column) {
            $this->path   = $path;
            $this->line   = $line;
            $this->column = $column;
        }

        /**
         * @param string $message
         * @return string
         */
        public function format($message) {
            return "$this->path($this->line,$this->column): $message\n";
        }
    }

    class ErrorReceiver {
        public function add($message, CodeLoc $loc) {
            print $loc->format($message);
        }
    }

    class File {
        /**
         * @param string[]      $paths
         * @param ErrorReceiver $errors
         * @return File[]
         */
        public static function parse(array $paths, ErrorReceiver $errors):array {
            /**
             * @var Parser\ParsedFile[] $parsed
             * @var self[]              $result
             */
            $parsed  = [];
            $defined = new Parser\GlobalDefinedNames;
            $result  = [];
            foreach ($paths as $path) {
                $file = new Parser\ParsedFile($path, $errors);
                $defined->addNodes($file->nodes);
                $parsed[] = $file;
            }
            foreach ($parsed as $file) {
                $self           = new self();
                $self->contents = (new Parser\Parser($file, $defined))->parseStmts($file->nodes);
                $self->path     = $file->path;
                $self->shebang  = $file->shebang;
                $result[]       = $self;
            }
            return $result;
        }

        /** @var string */
        private $path;
        /** @var string */
        private $shebang = '';
        /** @var Stmt\Stmt */
        private $contents;

        public function path() {
            return $this->path;
        }

        /**
         * @return string
         * @throws \Exception
         */
        public function unparse() {
            $nodes = [];

            $currentNamespace = null;
            $currentNodes     = [];

            foreach ($this->contents->split() as $stmt) {
                $namespaces = $stmt->namespaces();
                if (count($namespaces) > 1) {
                    throw new \Exception('Cant unparse single statement defining symbols in multiple namespaces');
                }

                $stmtNodes     = $stmt->unparse();
                $stmtNamespace = $namespaces ? $namespaces[0] : null;

                if (
                    $currentNamespace === null ||
                    $stmtNamespace === null ||
                    $stmtNamespace === $currentNamespace ||
                    !$currentNodes
                ) {
                    $currentNamespace = $stmtNamespace;
                    $currentNodes     = array_merge($currentNodes, $stmtNodes);
                } else {
                    $nodes[] = new \PhpParser\Node\Stmt\Namespace_(
                        $currentNamespace ? new \PhpParser\Node\Name($currentNamespace) : null,
                        $currentNodes
                    );

                    $currentNamespace = null;
                    $currentNodes     = [];
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

            return $this->shebang . (new \PhpParser\PrettyPrinter\Standard())->prettyPrintFile($nodes);
        }
    }

    abstract class Node {
    }
}

namespace JesseSchalken\PhpTypeChecker\Parser {

    use JesseSchalken\MagicUtils\DeepClone;
    use JesseSchalken\PhpTypeChecker\Node\CodeLoc;
    use JesseSchalken\PhpTypeChecker\Node\ErrorReceiver;
    use JesseSchalken\PhpTypeChecker\Node\Expr;
    use JesseSchalken\PhpTypeChecker\Node\Stmt;
    use JesseSchalken\PhpTypeChecker\Node\Type;
    use function JesseSchalken\MagicUtils\clone_ref;

    abstract class DefinedNames {
        private $names = [];

        /**
         * @param string[] $names
         */
        public function __construct($names = []) {
            foreach ($names as $name) {
                $this->add($name);
            }
        }

        /**
         * @param string $name
         * @return bool
         */
        public final function has($name) {
            return isset($this->names[$this->normalize($name)]);
        }

        /**
         * @param string $name
         */
        public final function add($name) {
            $this->names[$this->normalize($name)] = true;
        }

        /**
         * @param string $name
         * @return string
         */
        abstract protected function normalize($name);

        /**
         * @param string $prefix
         * @return string
         */
        public final function create($prefix) {
            for ($i = 0; $name = $prefix . $i, $this->has($name); $i++) {
            }

            $this->add($name);
            return $name;
        }
    }

    abstract class Uses {
        /** @var \PhpParser\Node\Name */
        private $namespace;
        /** @var \PhpParser\Node\Name[] */
        private $uses;
        /** @var DefinedNames */
        private $defined;

        public function __construct(\PhpParser\Node\Name $namespace, DefinedNames $defined) {
            $this->namespace = $namespace;
            $this->defined   = $defined;
        }

        /**
         * @param \PhpParser\Node\Name $name
         * @param Uses                 $classes
         * @return \PhpParser\Node\Name
         */
        public final function resolve(\PhpParser\Node\Name $name, Uses $classes) {
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

        /**
         * @param string $alias
         * @return \PhpParser\Node\Name
         */
        private function resolveUnqualified($alias) {
            if (isset($this->uses[$this->normalize($alias)])) {
                return $this->uses[$this->normalize($alias)];
            } else {
                return $this->resolveDefault($alias);
            }
        }

        /**
         * @param string $alias
         * @return \PhpParser\Node\Name
         */
        protected function resolveDefault($alias) {
            return \PhpParser\Node\Name::concat($this->namespace, $alias);
        }

        /**
         * @param \PhpParser\Node\Name $name
         * @param string|null          $alias
         */
        public final function add(\PhpParser\Node\Name $name, $alias = null) {
            $this->uses[$this->normalize($alias ?: $name->getLast())] = $name;
        }

        /**
         * @param \PhpParser\Node\Name $name
         * @return bool
         */
        protected final function defined(\PhpParser\Node\Name $name) {
            return $this->defined->has($name->toString());
        }

        /**
         * @param string $alias
         * @return string
         */
        protected function normalize($alias) {
            return $alias;
        }
    }

    class ClassUses extends Uses {
        protected function normalize($alias) {
            return strtolower($alias);
        }
    }

    class FunctionUses extends Uses {
        protected function normalize($alias) {
            return strtolower($alias);
        }

        protected function resolveDefault($alias) {
            $local = parent::resolveDefault($alias);

            return $this->defined($local) ? $local : new \PhpParser\Node\Name([$alias]);
        }
    }

    class ConstantUses extends Uses {
        protected function resolveDefault($alias) {
            $local = parent::resolveDefault($alias);

            return $this->defined($local) ? $local : new \PhpParser\Node\Name([$alias]);
        }
    }

    class DefinedNamesConstants extends DefinedNames {
        protected function normalize($name) {
            // $name is the name of the constant including the namespace.
            // Namespaces are case insensitive, but constants are case sensitive,
            // therefire split the name after the last "\" and strtolower() the left side.
            $pos = strrpos($name, '\\');
            $pos = $pos === false ? 0 : $pos + 1;

            $prefix   = substr($name, 0, $pos);
            $constant = substr($name, $pos);

            return strtolower($prefix) . $constant;
        }
    }

    class DefinedNamesCaseInsensitive extends DefinedNames {
        protected function normalize($name) {
            return strtolower($name);
        }
    }

    class DefinedNamesCaseSensitive extends DefinedNames {
        protected function normalize($name) {
            return $name;
        }
    }

    /**
     * @param \PhpParser\Node $node
     * @return \PhpParser\Node[]
     */
    function node_sub_nodes(\PhpParser\Node $node):array {
        $result = [];
        foreach ($node->getSubNodeNames() as $prop) {
            $value = $node->$prop;
            if (is_array($value)) {
                foreach ($value as $value2) {
                    if ($value2 instanceof \PhpParser\Node) {
                        $result[] = $value2;
                    }
                }
            } elseif ($value instanceof \PhpParser\Node) {
                $result[] = $value;
            }
        }
        return $result;
    }

    class GlobalDefinedNames {
        use DeepClone;

        /** @var DefinedNames */
        public $classes;
        /** @var DefinedNames */
        public $constants;
        /** @var DefinedNames */
        public $functions;

        public function __construct() {
            $this->classes   = new DefinedNamesCaseInsensitive;
            $this->constants = new DefinedNamesConstants;
            $this->functions = new DefinedNamesCaseInsensitive;
        }

        /**
         * @param \PhpParser\Node[] $nodes
         * @param string            $prefix
         */
        public function addNodes(array $nodes, $prefix = '') {
            foreach ($nodes as $node) {
                $this->addNode($node, $prefix);
            }
        }

        /**
         * @param \PhpParser\Node $node
         * @param string          $prefix
         * @return void
         */
        public function addNode(\PhpParser\Node $node, $prefix = '') {
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

    class ParsedFile {
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

        /**
         * @param string        $path
         * @param ErrorReceiver $errors
         */
        public function __construct($path, ErrorReceiver $errors) {
            $this->path     = $path;
            $this->contents = file_get_contents($path);
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

        /**
         * @param int|null $offset
         * @return int
         */
        public function offsetToColumn($offset = null) {
            if ($offset === null) {
                return 1;
            } else {
                $code      = $this->contents;
                $lineStart = strrpos($code, "\n", $offset - strlen($code));
                $lineStart = $lineStart === false ? 0 : $lineStart + 1;

                return $offset - $lineStart + 1;
            }
        }
    }

    class Parser {

        /** @var \PhpParser\Node\Name */
        private $namespace;
        /** @var Uses */
        private $useFunction;
        /** @var Uses This is used for all four of classes, interfaces, traits and namespaces */
        private $useClass;
        /** @var Uses */
        private $useConstant;

        /** @var Stmt\Block[] */
        private $finallys = [];

        /** @var bool */
        private $returnRef = false;

        /** @var GlobalDefinedNames */
        private $globals;

        /** @var DefinedNames */
        private $locals;

        /** @var ParsedFile */
        private $file;

        /** @var string|null */
        private $class;
        /** @var string|null */
        private $trait;
        /** @var string|null */
        private $parent;
        /** @var string|null */
        private $function;

        /**
         * @param ParsedFile         $file
         * @param GlobalDefinedNames $globals
         */
        public function __construct($file, GlobalDefinedNames $globals) {
            $this->globals     = $globals;
            $this->locals      = new DefinedNamesCaseSensitive();
            $this->finallys[0] = new Stmt\Block();
            $this->file        = $file;
            $this->resetNamespace();
        }

        private function resetNamespace(\PhpParser\Node\Name $name = null) {
            $this->namespace   = $name ?: new \PhpParser\Node\Name([]);
            $this->useClass    = new ClassUses($this->namespace, $this->globals->classes);
            $this->useFunction = new FunctionUses($this->namespace, $this->globals->functions);
            $this->useConstant = new ConstantUses($this->namespace, $this->globals->constants);
        }

        private function resetMagicConstants() {
            $this->class    = null;
            $this->trait    = null;
            $this->parent   = null;
            $this->function = null;
        }

        public function __clone() {
            clone_ref($this->finallys);
            clone_ref($this->useFunction);
            clone_ref($this->useClass);
            clone_ref($this->useConstant);
            clone_ref($this->class);
            clone_ref($this->trait);
            clone_ref($this->parent);
            clone_ref($this->function);
        }

        /**
         * @param \PhpParser\Node\Name $name
         * @return Expr\AbstractClassName
         * @throws \Exception
         */
        private function resolveClass(\PhpParser\Node\Name $name):Expr\AbstractClassName {
            if (
                !$name->isFullyQualified() &&
                !$name->isRelative() &&
                !$name->isQualified()
            ) {
                switch (strtolower($name->getFirst())) {
                    case 'self':
                        if ($this->class) {
                            return new Expr\ClassName($this->class);
                        } else {
                            throw new \Exception('Cannot use "self" outside a class');
                        }
                    case 'parent':
                        if ($this->parent) {
                            return new Expr\ClassName($this->parent);
                        } else {
                            throw new \Exception('Cannot use "parent" without a parent class');
                        }
                    case 'static':
                        return new Expr\StaticClassName();
                }
            }

            return new Expr\ClassName($this->useClass->resolve($name, $this->useClass)->toString());
        }

        /**
         * @param \PhpParser\Node\Name $name
         * @return string
         */
        private function resolveConst(\PhpParser\Node\Name $name) {
            return $this->useConstant->resolve($name, $this->useClass)->toString();
        }

        /**
         * @param \PhpParser\Node\Name $name
         * @return string
         */
        private function resolveFunction(\PhpParser\Node\Name $name) {
            return $this->useFunction->resolve($name, $this->useClass)->toString();
        }

        /**
         * @param \PhpParser\Node\Stmt[] $nodes
         * @return Stmt\Block
         */
        public function parseStmts(array $nodes):Stmt\Block {
            $stmts = new Stmt\Block();
            foreach ($nodes as $node) {
                $stmts->add($this->parseStmt($node));
            }
            return $stmts;
        }

        /**
         * @return string
         */
        private function newUnusedvariable() {
            return $this->locals->create('_');
        }

        private function getFinally() {
            return new Stmt\Block($this->finallys);
        }

        private function hasFinally() {
            return $this->getFinally()->split() ? true : false;
        }

        private function parseStmt(\PhpParser\Node $node):Stmt\Stmt {
            if ($node instanceof \PhpParser\Node\Expr) {
                return $this->parseExpr($node);
            } elseif ($node instanceof \PhpParser\Node\Stmt\If_) {
                $false = $this->parseStmts($node->else ? $node->else->stmts : []);

                foreach (array_reverse($node->elseifs) as $elseIf) {
                    $false = new Stmt\If_(
                        $this->parseExpr($elseIf->cond),
                        $this->parseStmts($elseIf->stmts),
                        $false
                    );
                }

                return new Stmt\If_(
                    $this->parseExpr($node->cond),
                    $this->parseStmts($node->stmts),
                    $false
                );
            } elseif ($node instanceof \PhpParser\Node\Stmt\Return_) {
                $expr = $this->parseExprNull($node->expr);

                if ($this->hasFinally()) {
                    $stmts = [];
                    if ($expr) {
                        $var     = new Expr\Variable(new Expr\Literal($this->newUnusedvariable()));
                        $stmts[] = new Expr\BinOp($var, $this->returnRef ? Expr\BinOp::ASSIGN_REF : Expr\BinOp::ASSIGN, $expr);
                        $expr    = $var;
                    }
                    return new Stmt\Block(array_merge(
                        $stmts,
                        $this->finallys,
                        [new Stmt\Return_($expr)]
                    ));
                }

                return new Stmt\Return_($expr);
            } elseif ($node instanceof \PhpParser\Node\Stmt\Namespace_) {
                $copy = clone $this;
                $copy->resetNamespace($node->name);
                return $copy->parseStmts($node->stmts);
            } elseif ($node instanceof \PhpParser\Node\Stmt\Class_) {
                return $this->parseClass($node);
            } elseif ($node instanceof \PhpParser\Node\Stmt\Function_) {
                $name = $this->prefixName($node->name);
                $self = clone $this;
                $self->resetMagicConstants();
                $self->function = $name;
                return new Stmt\Function_(
                    $name,
                    $self->parseFunctionType($node),
                    $self->parseStmts($node->stmts)
                );
            } elseif ($node instanceof \PhpParser\Node\Stmt\Interface_) {
                $name = $this->prefixName($node->name);
                $self = clone $this;

                $self->resetMagicConstants();
                $self->class = $name;

                $extends = [];
                foreach ($node->extends as $extend) {
                    $extends[] = $self->resolveClass($extend)->toString();
                }
                return new Stmt\Interface_($name, $extends, $self->parseClassMembers($node));
            } elseif ($node instanceof \PhpParser\Node\Stmt\Trait_) {
                $name = $this->prefixName($node->name);
                $self = clone $this;

                $self->resetMagicConstants();
                $self->trait  = $name;
                $self->class  = 'TEMPORARY';
                $self->parent = 'TEMPORARY';
                return new Stmt\Trait_($name, $self->parseClassMembers($node));
            } elseif ($node instanceof \PhpParser\Node\Stmt\Use_) {
                $this->addUses($node->uses, $node->type);
                return new Stmt\Block();
            } elseif ($node instanceof \PhpParser\Node\Stmt\GroupUse) {
                $this->addUses($node->uses, $node->type, $node->prefix);
                return new Stmt\Block();
            } elseif ($node instanceof \PhpParser\Node\Stmt\Foreach_) {
                return new Stmt\Foreach_(
                    $this->parseExpr($node->expr),
                    $this->parseExprNull($node->keyVar),
                    $this->parseExpr($node->valueVar),
                    $node->byRef,
                    $this->parseStmts($node->stmts)
                );
            } elseif ($node instanceof \PhpParser\Node\Stmt\Echo_) {
                return new Stmt\Echo_($this->parseExprs($node->exprs));
            } elseif ($node instanceof \PhpParser\Node\Stmt\InlineHTML) {
                return new Stmt\InlineHTML($node->value);
            } elseif ($node instanceof \PhpParser\Node\Stmt\Const_) {
                $stmts = [];
                foreach ($node->consts as $const) {
                    $stmts[] = new Stmt\Const_(
                        $this->prefixName($const->name),
                        $this->parseExpr($const->value)
                    );
                }
                return new Stmt\Block($stmts);
            } elseif ($node instanceof \PhpParser\Node\Stmt\Throw_) {
                return new Stmt\Throw_($this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Stmt\Static_) {
                $stmts = [];
                foreach ($node->vars as $finallyVar) {
                    $stmts[] = new Stmt\StaticVar(
                        $finallyVar->name,
                        $this->parseExprNull($finallyVar->default)
                    );
                }
                return new Stmt\Block($stmts);
            } elseif ($node instanceof \PhpParser\Node\Stmt\For_) {
                return new Stmt\For_(
                    $this->parseExprs($node->init),
                    $this->parseExprs($node->cond),
                    $this->parseExprs($node->loop),
                    $this->parseStmts($node->stmts)
                );
            } elseif ($node instanceof \PhpParser\Node\Stmt\Break_) {
                if ($node->num === null) {
                    $levels = 1;
                } elseif ($node->num instanceof \PhpParser\Node\Scalar\LNumber) {
                    $levels = $node->num->value;
                } else {
                    throw new \Exception('"break" statement must use a constant operand');
                }

                return new Stmt\Block(array_merge(
                    array_slice($this->finallys, 0, $levels),
                    [new Stmt\Break_($levels)]
                ));
            } elseif ($node instanceof \PhpParser\Node\Stmt\Continue_) {
                if ($node->num === null) {
                    $levels = 1;
                } elseif ($node->num instanceof \PhpParser\Node\Scalar\LNumber) {
                    $levels = $node->num->value;
                } else {
                    throw new \Exception('"continue" statement must use a constant operand');
                }

                return new Stmt\Block(array_merge(
                    array_slice($this->finallys, 0, $levels),
                    [new Stmt\Continue_($levels)]
                ));
            } elseif ($node instanceof \PhpParser\Node\Stmt\Switch_) {
                $cases = [];
                foreach ($node->cases as $case) {
                    $cases[] = new Stmt\Case_(
                        $this->parseExprNull($case->cond),
                        $this->parseStmts($case->stmts)
                    );
                }
                return new Stmt\Switch_(
                    $this->parseExpr($node->cond),
                    $cases
                );
            } elseif ($node instanceof \PhpParser\Node\Stmt\Unset_) {
                return new Stmt\Unset_($this->parseExprs($node->vars));
            } elseif ($node instanceof \PhpParser\Node\Stmt\While_) {
                return new Stmt\While_($this->parseExpr($node->cond), $this->parseStmts($node->stmts));
            } elseif ($node instanceof \PhpParser\Node\Stmt\TryCatch) {
                if ($node->finallyStmts) {
                    $finally      = $this->parseStmts($node->finallyStmts);
                    $finallyVar   = new Expr\Variable(new Expr\Literal($this->newUnusedvariable()));
                    $exceptionVar = new Expr\Variable(new Expr\Literal($excVar = $this->newUnusedvariable()));

                    $self = clone $this;
                    $self->finallys[0]->add(new Stmt\Block([
                        new Expr\BinOp($finallyVar, Expr\BinOp::ASSIGN, new Expr\Literal(true)),
                        $finally,
                    ]));

                    return new Stmt\Block([
                        new Expr\BinOp($finallyVar, Expr\BinOp::ASSIGN, new Expr\Literal(false)),
                        new Stmt\Try_($self->parseTryCatch($node), [
                            new Stmt\Catch_('Exception', $excVar, new Stmt\Block()),
                        ]),
                        new Stmt\If_(
                            new Expr\UnOp(Expr\UnOp::BOOL_NOT, $finallyVar),
                            $finally,
                            new Stmt\Block()
                        ),
                        new Stmt\If_(
                            new Expr\Isset_([$exceptionVar]),
                            new Stmt\Throw_($exceptionVar),
                            new Stmt\Block()
                        ),
                    ]);
                } else {
                    return $this->parseTryCatch($node);
                }
            } elseif ($node instanceof \PhpParser\Node\Stmt\Do_) {
                return new Stmt\DoWhile(
                    $this->parseStmts($node->stmts),
                    $this->parseExpr($node->cond)
                );
            } elseif ($node instanceof \PhpParser\Node\Stmt\Global_) {
                $stmts = new Stmt\Block();
                foreach ($node->vars as $var) {
                    $stmts->add(new Stmt\Global_($this->parseExpr($var)));
                }
                return $stmts;
            } elseif ($node instanceof \PhpParser\Node\Stmt\Label) {
                return new Stmt\Label_($node->name);
            } elseif ($node instanceof \PhpParser\Node\Stmt\Goto_) {
                return new Stmt\Goto_($node->name);
            } else {
                throw new \Exception('Unhandled statement type: ' . get_class($node));
            }
        }

        /**
         * @param string $name
         * @return string
         */
        private function prefixName($name) {
            return \PhpParser\Node\Name::concat($this->namespace, $name)->toString();
        }

        /**
         * @param \PhpParser\Node\Stmt\ClassLike $node
         * @return Stmt\AbstractClassMember[]
         * @throws \Exception
         */
        private function parseClassMembers(\PhpParser\Node\Stmt\ClassLike $node) {
            $stmts = [];
            foreach ($node->stmts as $stmt) {
                if ($stmt instanceof \PhpParser\Node\Stmt\ClassMethod) {
                    $stmts[] = new Stmt\Method_(
                        $stmt->name,
                        $this->parseFunctionType($stmt),
                        $stmt->stmts === null ? null : $this->parseStmts($stmt->stmts),
                        !!($stmt->type & \PhpParser\Node\Stmt\Class_::MODIFIER_FINAL),
                        $this->parseVisibility($stmt->type),
                        !!($stmt->type & \PhpParser\Node\Stmt\Class_::MODIFIER_STATIC)
                    );
                } else if ($stmt instanceof \PhpParser\Node\Stmt\ClassConst) {
                    foreach ($stmt->consts as $const) {
                        $stmts[] = new Stmt\ClassConstant(
                            $const->name,
                            $this->parseExpr($const->value)
                        );
                    }
                } else if ($stmt instanceof \PhpParser\Node\Stmt\Property) {
                    $visibility = $this->parseVisibility($stmt->type);
                    $static     = $stmt->type & \PhpParser\Node\Stmt\Class_::MODIFIER_STATIC;
                    foreach ($stmt->props as $prop) {
                        $stmts[] = new Stmt\Property(
                            $prop->name,
                            // TODO extract type from doc comment
                            new Type\Mixed(),
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
                    $useTrait = new Stmt\UseTrait($traits);
                    foreach ($stmt->adaptations as $adaption) {
                        if ($adaption instanceof \PhpParser\Node\Stmt\TraitUseAdaptation\Precedence) {
                            $insteadOf = [];
                            foreach ($adaption->insteadof as $name) {
                                $insteadOf[] = $this->resolveClass($name)->toString();
                            }
                            $useTrait->addInsteadOf(new Stmt\UseTraitInsteadof(
                                $this->resolveClass($adaption->trait)->toString(),
                                $adaption->method,
                                $insteadOf
                            ));
                        } elseif ($adaption instanceof \PhpParser\Node\Stmt\TraitUseAdaptation\Alias) {
                            $useTrait->addAlias(new Stmt\UseTraitAlias(
                                $adaption->newName !== null ? $adaption->newName : $adaption->method,
                                $adaption->method,
                                $adaption->trait ? $this->resolveClass($adaption->trait)->toString() : null,
                                $adaption->newModifier !== null ? $this->parseVisibility($adaption->newModifier) : null
                            ));
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

        /**
         * @param int $type
         * @return string
         */
        private function parseVisibility($type) {
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
         * @param \PhpParser\Node\FunctionLike $node
         * @return Stmt\FunctionSignature
         */
        private function parseFunctionType(\PhpParser\Node\FunctionLike $node) {
            // TODO extract types from doc comment
            // TODO and make sure they're compatible with any real type hints
            $params = [];
            foreach ($node->getParams() as $param) {
                $params[] = new Stmt\FunctionParam(
                    $param->name,
                    $this->parseExprNull($param->default),
                    $param->byRef,
                    $param->variadic,
                    $this->parseType($param->type)
                );
            }
            return new Stmt\FunctionSignature(
                $node->returnsByRef(),
                $params,
                $this->parseType($node->getReturnType())
            );
        }

        /**
         * @param null|string|\PhpParser\Node\Name $type
         * @return Type\Type
         * @throws \Exception
         */
        private function parseType($type):Type\Type {
            if ($type === null) {
                return new Type\Mixed;
            } else if (is_string($type)) {
                switch ($type) {
                    case 'int':
                    case 'integer':
                        return new Type\SimpleType(Type\SimpleType::INT);
                    case 'string':
                        return new Type\SimpleType(Type\SimpleType::STRING);
                    case 'double':
                    case 'float':
                        return new Type\SimpleType(Type\SimpleType::FLOAT);
                    case 'bool':
                    case 'boolean':
                        return new Type\SimpleType(Type\SimpleType::BOOL);
                    case 'null':
                    case 'void':
                        return new Type\SimpleType(Type\SimpleType::NULL);
                    case 'object':
                        return new Type\SimpleType(Type\SimpleType::OBJECT);
                    case 'resource':
                        return new Type\SimpleType(Type\SimpleType::RESOURCE);
                    case 'array':
                        return new Type\SimpleType(Type\SimpleType::ARRAY);
                    default:
                        throw new \Exception('Invalid simple type: ' . $type);
                }
            } else if ($type instanceof \PhpParser\Node\Name) {
                return new Type\ObjectType($this->resolveClass($type)->toString());
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
        private function addUses(array $uses, $type_, $prefix = null) {
            foreach ($uses as $use) {
                $name = $prefix ? \PhpParser\Node\Name::concat($prefix, $use->name) : $use->name;
                $type = $use->type === \PhpParser\Node\Stmt\Use_::TYPE_UNKNOWN ? $type_ : $use->type;
                switch ($type) {
                    case \PhpParser\Node\Stmt\Use_::TYPE_CONSTANT:
                        $this->useConstant->add($name, $use->alias);
                        break;
                    case \PhpParser\Node\Stmt\Use_::TYPE_FUNCTION:
                        $this->useFunction->add($name, $use->alias);
                        break;
                    case \PhpParser\Node\Stmt\Use_::TYPE_NORMAL:
                        $this->useClass->add($name, $use->alias);
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
         * @return Expr\Expr
         */
        private function parseExprString($node) {
            return is_string($node) ? new Expr\Literal($node) : $this->parseExpr($node);
        }

        /**
         * @param \PhpParser\Node\Expr|\PhpParser\Node\Name $node
         * @return Expr\Expr
         * @throws \Exception
         */
        private function parseExprClass($node) {
            return $node instanceof \PhpParser\Node\Name
                ? $this->resolveClass($node)
                : $this->parseExpr($node);
        }

        private function parseExpr(\PhpParser\Node\Expr $node):Expr\Expr {
            if ($node instanceof \PhpParser\Node\Expr\Variable) {
                return new Expr\Variable($this->parseExprString($node->name));
            } elseif ($node instanceof \PhpParser\Node\Expr\ConstFetch) {
                $name = $this->resolveConst($node->name);
                switch (strtolower($name)) {
                    case 'true':
                        return new Expr\Literal(true);
                    case 'false':
                        return new Expr\Literal(false);
                    case 'null':
                        return new Expr\Literal(null);
                    default:
                        return new Expr\ConstFetch($name);
                }
            } elseif ($node instanceof \PhpParser\Node\Expr\Assign) {
                return new Expr\BinOp(
                    $this->parseExpr($node->var),
                    Expr\BinOp::ASSIGN,
                    $this->parseExpr($node->expr)
                );
            } elseif ($node instanceof \PhpParser\Node\Scalar\LNumber) {
                return new Expr\Literal($node->value);
            } elseif ($node instanceof \PhpParser\Node\Scalar\DNumber) {
                return new Expr\Literal($node->value);
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

                return new Expr\Include_($this->parseExpr($node->expr), $require, $once);
            } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp\Concat) {
                return new Expr\BinOp(
                    $this->parseExpr($node->left),
                    Expr\BinOp::CONCAT,
                    $this->parseExpr($node->right)
                );
            } elseif ($node instanceof \PhpParser\Node\Scalar\MagicConst) {
                $type = $node->getName();
                $line = $node->getAttribute('startLine');
                return new Expr\MagicConst($type, $this->getMagicConstValue($type, $line));
            } elseif ($node instanceof \PhpParser\Node\Scalar\String_) {
                return new Expr\Literal($node->value);
            } elseif ($node instanceof \PhpParser\Node\Expr\StaticCall) {
                return new Expr\StaticCall(
                    $this->parseArgs($node->args),
                    $this->parseExprClass($node->class),
                    $this->parseExprString($node->name)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\FuncCall) {
                $function = $node->name;
                $function = $function instanceof \PhpParser\Node\Name
                    ? new Expr\Literal($this->resolveFunction($function))
                    : $this->parseExpr($function);

                return new Expr\FunctionCall($function, $this->parseArgs($node->args));
            } elseif ($node instanceof \PhpParser\Node\Expr\Array_) {
                $items = [];
                foreach ($node->items as $item) {
                    $items[] = new Expr\ArrayItem(
                        $this->parseExprNull($item->key),
                        $this->parseExpr($item->value),
                        $item->byRef
                    );
                }
                return new Expr\Array_($items);
            } elseif ($node instanceof \PhpParser\Node\Expr\Empty_) {
                return new Expr\UnOp(Expr\UnOp::EMPTY, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\ArrayDimFetch) {
                return new Expr\ArrayAccess(
                    $this->parseExpr($node->var),
                    $this->parseExprNull($node->dim)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\MethodCall) {
                return new Expr\MethodCall(
                    $this->parseArgs($node->args),
                    $this->parseExpr($node->var),
                    $this->parseExprString($node->name)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\New_) {
                $class = $node->class;
                if ($class instanceof \PhpParser\Node\Stmt\Class_) {
                    $class   = $this->parseClass($class, $this->globals->classes->create($this->prefixName('class')));
                    $stmts[] = $class;
                    $class   = new Expr\Literal($class->name());
                } else {
                    $class = $this->parseExprClass($class);
                }

                return new Expr\New_($class, $this->parseArgs($node->args));
            } elseif ($node instanceof \PhpParser\Node\Expr\BooleanNot) {
                return new Expr\UnOp(Expr\UnOp::BOOL_NOT, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\Print_) {
                return new Expr\UnOp(Expr\UnOp::PRINT, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\Closure) {
                $uses = [];
                foreach ($node->uses as $use) {
                    $uses[] = new Expr\ClosureUse($use->var, $use->byRef);
                }
                return new Expr\Closure(
                    $node->static,
                    $this->parseFunctionType($node),
                    $uses,
                    $this->parseStmts($node->stmts)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\Ternary) {
                return new Expr\Ternary(
                    $this->parseExpr($node->cond),
                    $this->parseExprNull($node->if),
                    $this->parseExpr($node->else)
                );
            } elseif ($node instanceof \PhpParser\Node\Scalar\EncapsedStringPart) {
                return new Expr\Literal($node->value);
            } elseif ($node instanceof \PhpParser\Node\Scalar\Encapsed) {
                $exprs = [];
                foreach ($node->parts as $part) {
                    $exprs[] = $this->parseExpr($part);
                }
                return new Expr\ConcatMany($exprs);
            } elseif ($node instanceof \PhpParser\Node\Expr\StaticPropertyFetch) {
                $class = $node->class;
                $prop  = $node->name;

                if ($class instanceof \PhpParser\Node\Name) {
                    $class = $this->resolveClass($class);
                } elseif ($class instanceof \PhpParser\Node\Expr) {
                    $class = $this->parseExpr($class);
                } else {
                    throw new \Exception('huh?');
                }

                if (is_string($prop)) {
                    $prop = new Expr\Literal($prop);
                } elseif ($prop instanceof \PhpParser\Node\Expr) {
                    $prop = $this->parseExpr($prop);
                } else {
                    throw new \Exception('huh?');
                }

                return new Expr\StaticPropertyAccess($class, $prop);
            } elseif ($node instanceof \PhpParser\Node\Expr\Isset_) {
                return new Expr\Isset_($this->parseExprs($node->vars));
            } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp) {
                return $this->parseBinaryOp($node);
            } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp) {
                return $this->parseAssignOp($node);
            } elseif ($node instanceof \PhpParser\Node\Expr\ErrorSuppress) {
                return new Expr\UnOp(Expr\UnOp::SUPPRESS, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\PropertyFetch) {
                return new Expr\PropertyAccess(
                    $this->parseExpr($node->var),
                    $this->parseExprString($node->name)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\Exit_) {
                return new Expr\Exit_($this->parseExprNull($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\Eval_) {
                return new Expr\UnOp(Expr\UnOp::EVAL, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\Cast) {
                return $this->parseCast($node);
            } elseif ($node instanceof \PhpParser\Node\Expr\Instanceof_) {
                return new Expr\BinOp(
                    $this->parseExpr($node->expr),
                    Expr\BinOp:: INSTANCEOF,
                    $this->parseExprClass($node->class)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\Clone_) {
                return new Expr\UnOp(Expr\UnOp::CLONE, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\Yield_) {
                return new Expr\Yield_(
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
                    return new Expr\ClassConstFetch($this->parseExprClass($node->class), $node->name);
                }
            } elseif ($node instanceof \PhpParser\Node\Expr\UnaryMinus) {
                return new Expr\UnOp(Expr\UnOp::NEGATE, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\UnaryPlus) {
                return new Expr\UnOp(Expr\UnOp::PLUS, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\PostInc) {
                return new Expr\UnOp(Expr\UnOp::POST_INC, $this->parseExpr($node->var));
            } elseif ($node instanceof \PhpParser\Node\Expr\PreInc) {
                return new Expr\UnOp(Expr\UnOp::PRE_INC, $this->parseExpr($node->var));
            } elseif ($node instanceof \PhpParser\Node\Expr\PostDec) {
                return new Expr\UnOp(Expr\UnOp::POST_DEC, $this->parseExpr($node->var));
            } elseif ($node instanceof \PhpParser\Node\Expr\PreDec) {
                return new Expr\UnOp(Expr\UnOp::PRE_DEC, $this->parseExpr($node->var));
            } elseif ($node instanceof \PhpParser\Node\Expr\List_) {
                $exprs = [];
                foreach ($node->vars as $v) {
                    $exprs[] = $this->parseExprNull($v);
                }
                return new Expr\List_($exprs);
            } elseif ($node instanceof \PhpParser\Node\Expr\AssignRef) {
                return new Expr\BinOp(
                    $this->parseExpr($node->var),
                    Expr\BinOp::ASSIGN_REF,
                    $this->parseExpr($node->expr)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\BitwiseNot) {
                return new Expr\UnOp(Expr\UnOp::BIT_NOT, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\ShellExec) {
                $exprs = [];
                foreach ($node->parts as $part) {
                    $exprs[] = $this->parseExpr($part);
                }
                return new Expr\ShellExec($exprs);
            } else {
                throw new \Exception('Unhandled expression type: ' . get_class($node));
            }
        }

        private function parseCast(\PhpParser\Node\Expr\Cast $node):Expr\Expr {
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

            return new Expr\Cast($type, $this->parseExpr($node->expr));
        }

        private function parseAssignOp(\PhpParser\Node\Expr\AssignOp $node):Expr\Expr {
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
            return new Expr\BinOp($left, $type, $right);
        }

        private function parseBinaryOp(\PhpParser\Node\Expr\BinaryOp $node):Expr\Expr {
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
            return new Expr\BinOp($left, $type, $right);
        }

        /**
         * @param string $type
         * @param int    $line
         * @return string
         * @throws \Exception
         */
        private function getMagicConstValue($type, $line) {
            switch ($type) {
                case Expr\MagicConst::LINE:
                    return $line;
                case Expr\MagicConst::FILE:
                    return realpath($this->file->path);
                case Expr\MagicConst::DIR:
                    return dirname(realpath($this->file->path));
                case Expr\MagicConst::FUNCTION:
                    return (string)$this->function;
                case Expr\MagicConst::CLASS_:
                    return (string)$this->class;
                case Expr\MagicConst::TRAIT:
                    return (string)$this->class;
                case Expr\MagicConst::METHOD:
                    return $this->class ? "$this->class::$this->function" : "$this->function";
                case Expr\MagicConst::NAMESPACE:
                    return $this->namespace->toString();
                default:
                    throw new \Exception("Invalid magic constant type: $type");
            }
        }

        /**
         * @param \PhpParser\Node\Arg[] $args
         * @return Expr\CallArg[]
         * @throws \Exception
         */
        private function parseArgs(array $args) {
            $result = [];
            foreach ($args as $arg) {
                $result[] = new Expr\CallArg(
                    $this->parseExpr($arg->value),
                    $arg->byRef,
                    $arg->unpack
                );
            }
            return $result;
        }

        /**
         * @param \PhpParser\Node\Stmt\TryCatch $node
         * @return Stmt\Stmt
         */
        private function parseTryCatch(\PhpParser\Node\Stmt\TryCatch $node):Stmt\Stmt {
            $result = $this->parseStmts($node->stmts);

            if ($node->catches) {
                $catches = [];
                foreach ($node->catches as $catch) {
                    $catches[] = new Stmt\Catch_(
                        $this->resolveClass($catch->type)->toString(),
                        $catch->var,
                        $this->parseStmts($catch->stmts)
                    );
                }
                $result = new Stmt\Try_($result, $catches);
            }

            return $result;
        }

        private function parseClass(\PhpParser\Node\Stmt\Class_ $node, $name = null):Stmt\Class_ {
            $name       = $name ?: $this->prefixName($node->name);
            $parent     = $node->extends ? $this->resolveClass($node->extends)->toString() : null;
            $implements = [];
            foreach ($node->implements as $impl) {
                $implements[] = $this->resolveClass($impl)->toString();
            }

            $self = clone $this;
            $self->resetMagicConstants();
            $self->class  = $name;
            $self->parent = $parent;
            return new Stmt\Class_(
                $name,
                $self->parseClassMembers($node),
                $parent,
                $implements,
                $node->type & \PhpParser\Node\Stmt\Class_::MODIFIER_ABSTRACT ? true : false,
                $node->type & \PhpParser\Node\Stmt\Class_::MODIFIER_FINAL ? true : false
            );
        }
    }
}

namespace JesseSchalken\PhpTypeChecker\Node\Stmt {

    use JesseSchalken\PhpTypeChecker\Node\Expr;
    use JesseSchalken\PhpTypeChecker\Node\Node;
    use JesseSchalken\PhpTypeChecker\Node\Type;
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

        public function namespaces():array {
            $namespaces = [];
            foreach ($this->subStmts() as $stmt) {
                foreach ($stmt->namespaces() as $namespace) {
                    $namespaces[] = $namespace;
                }
            }
            return array_unique($namespaces);
        }

        /**
         * @return \PhpParser\Node[]
         */
        public abstract function unparse():array;
    }

    final class Block extends Stmt {
        /** @var Stmt[] */
        private $stmts;

        /**
         * @param Stmt[] $stmts
         */
        public function __construct(array $stmts = []) {
            $this->stmts = $stmts;
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

        public function add(Stmt $stmt) {
            $this->stmts[] = $stmt;
        }

        public function subStmts():array {
            return $this->stmts;
        }

        public function unparse():array {
            $nodes = [];
            foreach ($this->stmts as $stmt) {
                foreach ($stmt->unparse() as $node) {
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
    }

    class DoWhile extends SingleStmt {
        /** @var Stmt */
        private $body;
        /** @var Expr\Expr */
        private $cond;

        /**
         * @param Stmt      $body
         * @param Expr\Expr $cond
         */
        public function __construct(Stmt $body, Expr\Expr $cond) {
            $this->body = $body;
            $this->cond = $cond;
        }

        public function subStmts():array {
            return [$this->body, $this->cond];
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\While_(
                $this->cond->unparse_(),
                $this->body->unparse()
            )];
        }
    }

    class If_ extends SingleStmt {
        /** @var Expr\Expr */
        private $cond;
        /** @var Stmt */
        private $true;
        /** @var Stmt */
        private $false;

        public function __construct(Expr\Expr $cond, Stmt $true, Stmt $false) {
            $this->cond  = $cond;
            $this->true  = $true;
            $this->false = $false;
        }

        public function subStmts():array {
            return [$this->cond, $this->true, $this->false];
        }

        public function unparse():array {
            $elseIfs = [];
            $else    = $this->false->unparse();

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

            return [new \PhpParser\Node\Stmt\If_(
                $this->cond->unparse_(),
                [
                    'stmts'   => $this->true->unparse(),
                    'elseifs' => $elseIfs,
                    'else'    => $else ? new \PhpParser\Node\Stmt\Else_($else) : null,
                ]
            )];
        }
    }

    class Return_ extends SingleStmt {
        /** @var Expr\Expr|null */
        private $expr;

        public function __construct(Expr\Expr $expr = null) {
            $this->expr = $expr;
        }

        public function subStmts():array {
            return $this->expr ? [$this->expr] : [];
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Return_($this->expr ? $this->expr->unparse_() : null)];
        }
    }

    class Visibility {
        /**
         * @param string $visibility
         * @return int
         * @throws \Exception
         */
        static function unparse($visibility) {
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

        /**
         * @param string $html
         */
        public function __construct($html) {
            $this->html = $html;
        }

        public function subStmts():array {
            return [];
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\InlineHTML($this->html)];
        }
    }

    abstract class Classish extends SingleStmt {
        private $name;

        /**
         * @param string $name
         */
        public function __construct($name) {
            $this->name = $name;
        }

        public function name() {
            return $this->name;
        }

        public function basename() {
            return remove_namespace($this->name);
        }

        /**
         * @return AbstractClassMember[]
         */
        public abstract function members();

        public function subStmts():array {
            $stmts = [];
            foreach ($this->members() as $member) {
                foreach ($member->subStmts() as $stmt) {
                    $stmts[] = $stmt;
                }
            }
            return $stmts;
        }

        public function namespaces():array {
            return array_merge(parent::namespaces(), [extract_namespace($this->name)]);
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
         * @param string        $name
         * @param ClassMember[] $members
         */
        public function __construct($name, array $members) {
            parent::__construct($name);
            $this->members = $members;
        }

        public function members() {
            return $this->members;
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Trait_(
                $this->basename(),
                $this->unparseMembers()
            )];
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
         * @param string        $name
         * @param ClassMember[] $members
         * @param string|null   $parent
         * @param string[]      $implements
         * @param bool          $abstract
         * @param bool          $final
         */
        public function __construct(
            $name,
            array $members,
            $parent = null,
            array $implements = [],
            $abstract,
            $final
        ) {
            parent::__construct($name);
            $this->members    = $members;
            $this->parent     = $parent;
            $this->implements = $implements;
            $this->abstract   = $abstract;
            $this->final      = $final;
        }

        public function makeAnonymous():Expr\Expr {
            return new If_(
                new Expr\UnOp(Expr\UnOp::BOOL_NOT, new Expr\FunctionCall(new Expr\Literal('class_exists'), [
                    new Expr\CallArg(new Expr\Literal($this->name()), false, false),
                    new Expr\CallArg(new Expr\Literal(false), false, false),
                ])),
                new Block([$this]),
                new Block()
            );
        }

        public function members() {
            return $this->members;
        }

        public function unparse():array {
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
            return [new \PhpParser\Node\Stmt\Class_(
                $this->basename(),
                [
                    'type'       => $type,
                    'extends'    => $this->parent ? new \PhpParser\Node\Name\FullyQualified($this->parent) : null,
                    'implements' => $implements,
                    'stmts'      => $this->unparseMembers(),
                ]
            )];
        }
    }

    class Interface_ extends Classish {
        /** @var Method_[] */
        private $methods = [];
        /** @var string[] */
        private $extends = [];

        /**
         * @param string    $name
         * @param string[]  $extends
         * @param Method_[] $methods
         */
        public function __construct($name, $extends, array $methods) {
            parent::__construct($name);
            $this->methods = $methods;
            $this->extends = $extends;
        }

        public function members() {
            return $this->methods;
        }

        public function unparse():array {
            $extends = [];
            foreach ($this->extends as $extend) {
                $extends[] = new \PhpParser\Node\Name\FullyQualified($extend);
            }
            return [new \PhpParser\Node\Stmt\Interface_(
                $this->basename(),
                [
                    'stmts'   => $this->unparseMembers(),
                    'extends' => $extends,
                ]
            )];
        }
    }

    /**
     * @param string $name
     * @return string
     */
    function extract_namespace($name) {
        $pos = strrpos($name, '\\');
        return $pos === false ? '' : substr($name, 0, $pos);
    }

    /**
     * @param string $name
     * @return string
     */
    function remove_namespace($name) {
        $pos = strrpos($name, '\\');
        return $pos === false ? $name : substr($name, $pos + 1);
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

        public function subStmts():array {
            $stmts = $this->type->subStmts();
            if ($this->body) {
                $stmts[] = $this->body;
            }
            return $stmts;
        }

        public function namespaces():array {
            return array_merge(parent::namespaces(), [extract_namespace($this->name)]);
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Function_(
                remove_namespace($this->name),
                array_replace($this->type->unparseAttributes(), [
                    'stmts' => $this->body->unparse(),
                ])
            )];
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

        /**
         * @param string    $name
         * @param Expr\Expr $value
         */
        public function __construct($name, Expr\Expr $value) {
            $this->name  = $name;
            $this->value = $value;
        }

        public function unparse():\PhpParser\Node {
            return new \PhpParser\Node\Stmt\ClassConst([
                new \PhpParser\Node\Const_(
                    $this->name,
                    $this->value->unparse_()
                ),
            ]);
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
         * @param string[] $traits
         */
        public function __construct(array $traits) {
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

        /**
         * @param string   $trait
         * @param string   $method
         * @param string[] $insteadOf
         */
        public function __construct($trait, $method, array $insteadOf) {
            $this->trait     = $trait;
            $this->method    = $method;
            $this->insteadOf = $insteadOf;
        }

        public function method() {
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
         * @param string      $alias
         * @param string      $method
         * @param null|string $trait
         * @param null|string $visibility
         */
        public function __construct($alias, $method, $trait, $visibility) {
            $this->alias      = $alias;
            $this->method     = $method;
            $this->trait      = $trait;
            $this->visibility = $visibility;
        }

        public function alias() {
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

        public final function modifiers() {
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
         * @param string         $name
         * @param Type\Type      $type
         * @param Expr\Expr|null $default
         * @param string         $visibility
         * @param bool           $static
         */
        public function __construct($name, Type\Type $type, Expr\Expr $default = null, $visibility, $static) {
            parent::__construct($visibility, $static);
            $this->name    = $name;
            $this->type    = $type;
            $this->default = $default;
        }

        public function subStmts():array {
            return $this->default ? $this->default->subStmts() : [];
        }

        public function unparse():\PhpParser\Node {
            return new \PhpParser\Node\Stmt\Property(
                $this->modifiers(),
                [new \PhpParser\Node\Stmt\PropertyProperty(
                    $this->name,
                    $this->default ? $this->default->unparse_() : null
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
         * @param bool            $returnRef
         * @param FunctionParam[] $params
         * @param Type\Type       $returnType
         */
        public function __construct($returnRef, array $params, Type\Type $returnType) {
            $this->returnRef  = $returnRef;
            $this->params     = $params;
            $this->returnType = $returnType;
        }

        public function subStmts() {
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
                'returnType' => $this->returnType->unparse(),
            ];
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

        public function subStmts():array {
            $stmts = $this->type->subStmts();
            if ($this->body) {
                $stmts[] = $this->body;
            }
            return $stmts;
        }

        public function unparse():\PhpParser\Node {
            $params = $this->type->unparseAttributes();
            $params = array_replace($params, [
                'type'  => $this->modifiers(),
                'stmts' => $this->body ? $this->body->unparse() : null,
            ]);
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
         * @param string         $name
         * @param Expr\Expr|null $default
         * @param bool           $passByRef
         * @param bool           $variadic
         * @param Type\Type      $type
         */
        public function __construct($name, Expr\Expr $default = null, $passByRef, $variadic, Type\Type $type) {
            $this->name      = $name;
            $this->default   = $default;
            $this->passByRef = $passByRef;
            $this->variadic  = $variadic;
            $this->type      = $type;
        }

        public function subStmts() {
            return $this->default ? [$this->default] : [];
        }

        public function unparse():\PhpParser\Node\Param {
            return new \PhpParser\Node\Param(
                $this->name,
                $this->default ? $this->default->unparse_() : null,
                $this->type->unparse(),
                $this->passByRef,
                $this->variadic
            );
        }
    }

    class Foreach_ extends SingleStmt {
        /** @var Expr\Expr */
        private $array;
        /** @var Expr\Expr|null */
        private $key;
        /** @var Expr\Expr */
        private $value;
        /** @var Stmt */
        private $body;
        /** @var bool */
        private $byRef;

        /**
         * @param Expr\Expr      $array
         * @param Expr\Expr|null $key
         * @param Expr\Expr      $value
         * @param bool           $byRef
         * @param Stmt           $body
         */
        public function __construct(Expr\Expr $array, Expr\Expr $key = null, Expr\Expr $value, $byRef, Stmt $body) {
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

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Foreach_(
                $this->array->unparse_(),
                $this->value->unparse_(),
                [
                    'keyVar' => $this->key ? $this->key->unparse_() : null,
                    'byRef'  => $this->byRef,
                    'stmts'  => $this->body->unparse(),
                ]
            )];
        }
    }

    class Echo_ extends SingleStmt {
        /** @var Expr\Expr[] */
        private $exprs;

        /**
         * @param Expr\Expr[] $exprs
         */
        public function __construct(array $exprs) {
            $this->exprs = $exprs;
        }

        public function subStmts():array {
            return $this->exprs;
        }

        public function unparse():array {
            $exprs = [];
            foreach ($this->exprs as $expr) {
                $exprs[] = $expr->unparse_();
            }
            return [new \PhpParser\Node\Stmt\Echo_($exprs)];
        }
    }

    class Const_ extends SingleStmt {
        /** @var string */
        private $name;
        /** @var Expr\Expr */
        private $value;

        /**
         * @param string    $name
         * @param Expr\Expr $value
         */
        public function __construct($name, Expr\Expr $value) {
            $this->name  = $name;
            $this->value = $value;
        }

        public function subStmts():array {
            return [$this->value];
        }

        public function namespaces():array {
            return array_merge(parent::namespaces(), [extract_namespace($this->name)]);
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Const_([
                new \PhpParser\Node\Const_(
                    remove_namespace($this->name),
                    $this->value->unparse_()
                ),
            ])];
        }
    }

    class Throw_ extends SingleStmt {
        /** @var Expr\Expr */
        private $expr;

        /**
         * @param Expr\Expr $expr
         */
        public function __construct(Expr\Expr $expr) {
            $this->expr = $expr;
        }

        public function subStmts():array {
            return [$this->expr];
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Throw_(
                $this->expr->unparse_()
            )];
        }
    }

    class StaticVar extends SingleStmt {
        /** @var string */
        private $name;
        /** @var Expr\Expr|null */
        private $value;

        /**
         * @param string         $name
         * @param Expr\Expr|null $value
         */
        public function __construct($name, Expr\Expr $value = null) {
            $this->name  = $name;
            $this->value = $value;
        }

        public function subStmts():array {
            return $this->value ? [$this->value] : [];
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Static_([
                new \PhpParser\Node\Stmt\StaticVar(
                    $this->name,
                    $this->value ? $this->value->unparse_() : null
                ),
            ])];
        }
    }

    class For_ extends SingleStmt {
        /** @var Expr\Expr[] */
        private $init = [];
        /** @var Expr\Expr[] */
        private $cond = [];
        /** @var Expr\Expr[] */
        private $loop = [];
        /** @var Stmt */
        private $body;

        /**
         * @param Expr\Expr[] $init
         * @param Expr\Expr[] $cond
         * @param Expr\Expr[] $loop
         * @param Stmt        $body
         */
        public function __construct(array $init, array $cond, array $loop, Stmt $body) {
            $this->init = $init;
            $this->cond = $cond;
            $this->loop = $loop;
            $this->body = $body;
        }

        public function subStmts():array {
            return array_merge(
                $this->init,
                $this->cond,
                $this->loop,
                [$this->body]
            );
        }

        public function unparse():array {
            $init = [];
            $cond = [];
            $loop = [];
            foreach ($this->init as $expr) {
                $init[] = $expr->unparse_();
            }
            foreach ($this->cond as $expr) {
                $cond[] = $expr->unparse_();
            }
            foreach ($this->loop as $expr) {
                $loop[] = $expr->unparse_();
            }
            return [new \PhpParser\Node\Stmt\For_([
                'init'  => $init,
                'cond'  => $cond,
                'loop'  => $loop,
                'stmts' => $this->body->unparse(),
            ])];
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

        public function subStmts():array {
            return [];
        }

        public function unparse():array {
            $levels = $this->levels == 1
                ? null
                : new \PhpParser\Node\Scalar\LNumber($this->levels);
            return [new \PhpParser\Node\Stmt\Break_($levels)];
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

        public function subStmts():array {
            return [];
        }

        public function unparse():array {
            $levels = $this->levels == 1
                ? null
                : new \PhpParser\Node\Scalar\LNumber($this->levels);
            return [new \PhpParser\Node\Stmt\Continue_($levels)];
        }
    }

    class Switch_ extends SingleStmt {
        /** @var Expr\Expr */
        private $expr;
        /** @var Case_[] */
        private $cases;

        /**
         * @param Expr\Expr $expr
         * @param Case_[]   $cases
         */
        public function __construct(Expr\Expr $expr, array $cases) {
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

        public function unparse():array {
            $cases = [];
            foreach ($this->cases as $case) {
                $cases[] = $case->unparse();
            }
            return [new \PhpParser\Node\Stmt\Switch_(
                $this->expr->unparse_(),
                $cases
            )];
        }
    }

    class Case_ extends Node {
        /** @var Expr\Expr|null */
        private $expr;
        /** @var Stmt */
        private $stmt;

        /**
         * @param Expr\Expr|null $expr
         * @param Stmt           $stmt
         */
        public function __construct(Expr\Expr $expr = null, Stmt $stmt) {
            $this->expr = $expr;
            $this->stmt = $stmt;
        }

        public function subStmts() {
            $stmts = [$this->stmt];
            if ($this->expr) {
                $stmts[] = $this->expr;
            }
            return $stmts;
        }

        public function unparse():\PhpParser\Node\Stmt\Case_ {
            return new \PhpParser\Node\Stmt\Case_(
                $this->expr ? $this->expr->unparse_() : null,
                $this->stmt->unparse()
            );
        }
    }

    class Unset_ extends SingleStmt {
        /** @var Expr\Expr[] */
        private $exprs;

        /**
         * @param Expr\Expr[] $exprs
         */
        public function __construct(array $exprs) {
            $this->exprs = $exprs;
        }

        public function subStmts():array {
            return $this->exprs;
        }

        public function unparse():array {
            $exprs = [];
            foreach ($this->exprs as $expr) {
                $exprs[] = $expr->unparse_();
            }
            return [new \PhpParser\Node\Stmt\Unset_($exprs)];
        }
    }

    class While_ extends SingleStmt {
        /** @var Expr\Expr */
        private $cond;
        /** @var Stmt */
        private $body;

        /**
         * @param Expr\Expr $cond
         * @param Stmt      $body
         */
        public function __construct(Expr\Expr $cond, Stmt $body) {
            $this->cond = $cond;
            $this->body = $body;
        }

        public function subStmts():array {
            return [$this->cond, $this->body];
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\While_(
                $this->cond->unparse_(),
                $this->body->unparse()
            )];
        }
    }

    class Try_ extends SingleStmt {
        /** @var Stmt */
        private $body;
        /** @var Catch_[] */
        private $catches;

        /**
         * @param Stmt     $body
         * @param Catch_[] $catches
         */
        public function __construct(Stmt $body, array $catches) {
            $this->body    = $body;
            $this->catches = $catches;
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

        public function unparse():array {
            $body   = $this->body->unparse();
            $cathes = [];
            foreach ($this->catches as $catch) {
                $cathes[] = $catch->unparse();
            }

            return !$cathes ? $body : [new \PhpParser\Node\Stmt\TryCatch(
                $body,
                $cathes,
                null
            )];
        }
    }

    class Catch_ extends Node {
        /** @var string */
        private $class;
        /** @var string */
        private $variable;
        /** @var Stmt */
        private $body;

        /**
         * @param string $class
         * @param string $variable
         * @param Stmt   $body
         */
        public function __construct($class, $variable, Stmt $body) {
            $this->class    = $class;
            $this->variable = $variable;
            $this->body     = $body;
        }

        public function subStmts() {
            return [$this->body];
        }

        public function unparse():\PhpParser\Node\Stmt\Catch_ {
            return new \PhpParser\Node\Stmt\Catch_(
                new \PhpParser\Node\Name\FullyQualified($this->class),
                $this->variable,
                $this->body->unparse()
            );
        }
    }

    class Global_ extends SingleStmt {
        /** @var Expr\Expr */
        private $expr;

        /**
         * @param Expr\Expr $expr
         */
        public function __construct(Expr\Expr $expr) {
            $this->expr = $expr;
        }

        public function subStmts():array {
            return [$this->expr];
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Global_([
                $this->expr->unparse_(),
            ])];
        }
    }

    class Label_ extends SingleStmt {
        /** @var string */
        private $name;

        /**
         * @param string $name
         */
        public function __construct($name) {
            $this->name = $name;
        }

        public function subStmts():array {
            return [];
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Label($this->name)];
        }
    }

    class Goto_ extends SingleStmt {
        /** @var string */
        private $name;

        /**
         * @param string $name
         */
        public function __construct($name) {
            $this->name = $name;
        }

        public function subStmts():array {
            return [];
        }

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Goto_($this->name)];
        }
    }
}

namespace JesseSchalken\PhpTypeChecker\Node\Expr {

    use JesseSchalken\PhpTypeChecker\Node\Node;
    use JesseSchalken\PhpTypeChecker\Node\Stmt;

    abstract class Expr extends Stmt\SingleStmt {
        public function isLValue() {
            return false;
        }

        public function unparse():array {
            return [$this->unparse_()];
        }

        public abstract function unparse_():\PhpParser\Node\Expr;

        /**
         * @return \PhpParser\Node\Expr|\PhpParser\Node\Name
         */
        public function unparseOrName() {
            return $this->unparse_();
        }

        /**
         * @return \PhpParser\Node\Expr|string
         */
        public function unparseOrString() {
            return $this->unparse_();
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

        public function subStmts():array {
            $stmts = [];
            foreach ($this->exprs as $expr) {
                if ($expr) {
                    $stmts[] = $expr;
                }
            }
            return $stmts;
        }

        public function unparse_():\PhpParser\Node\Expr {
            $exprs = [];
            /** @var Expr|null $expr */
            foreach ($this->exprs as $expr) {
                $exprs[] = $expr ? $expr->unparse_() : null;
            }
            return new \PhpParser\Node\Expr\List_($exprs);
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

        public function subStmts():array {
            $stmts = [];
            if ($this->key) {
                $stmts[] = $this->key;
            }
            if ($this->value) {
                $stmts[] = $this->value;
            }
            return $stmts;
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\Yield_(
                $this->value->unparse_(),
                $this->key ? $this->key->unparse_() : null
            );
        }
    }

    class ConstFetch extends Expr {
        /** @var string */
        private $name;

        /**
         * @param string $name
         */
        public function __construct($name) {
            $this->name = $name;
        }

        public function subStmts():array {
            return [];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\ConstFetch(new \PhpParser\Node\Name\FullyQualified($this->name));
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

        public function subStmts():array {
            return [$this->class];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\ClassConstFetch(
                $this->class->unparseOrName(),
                $this->const
            );
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

        public function subStmts():array {
            return [$this->name];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\Variable($this->name->unparseOrString());
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

        public function subStmts():array {
            return [$this->object, $this->property];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\PropertyFetch(
                $this->object->unparse_(),
                $this->property->unparseOrString()
            );
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

        public function subStmts():array {
            return [$this->class, $this->property];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\StaticPropertyFetch(
                $this->class->unparseOrName(),
                $this->property->unparseOrString()
            );
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

        public function subStmts():array {
            return $this->key ? [$this->array, $this->key] : [$this->array];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\ArrayDimFetch(
                $this->array->unparse_(),
                $this->key ? $this->key->unparse_() : null
            );
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

        public function subStmts():array {
            return [$this->expr];
        }

        public function unparse_():\PhpParser\Node\Expr {
            $type = $this->require
                ? ($this->once
                    ? \PhpParser\Node\Expr\Include_::TYPE_REQUIRE_ONCE
                    : \PhpParser\Node\Expr\Include_::TYPE_REQUIRE)
                : ($this->once
                    ? \PhpParser\Node\Expr\Include_::TYPE_INCLUDE_ONCE
                    : \PhpParser\Node\Expr\Include_::TYPE_INCLUDE);

            return new \PhpParser\Node\Expr\Include_($this->expr->unparse_(), $type);
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

        public function subStmts():array {
            return [];
        }

        public function unparse_():\PhpParser\Node\Expr {
            switch ($this->type) {
                case self::LINE:
                    return new \PhpParser\Node\Scalar\MagicConst\Line();
                case self::FILE:
                    return new \PhpParser\Node\Scalar\MagicConst\File();
                case self::DIR:
                    return new \PhpParser\Node\Scalar\MagicConst\Dir();
                case self::FUNCTION:
                    return new \PhpParser\Node\Scalar\MagicConst\Function_();
                case self::CLASS_:
                    return new \PhpParser\Node\Scalar\MagicConst\Class_();
                case self::TRAIT:
                    return new \PhpParser\Node\Scalar\MagicConst\Trait_();
                case self::METHOD:
                    return new \PhpParser\Node\Scalar\MagicConst\Method();
                case self::NAMESPACE:
                    return new \PhpParser\Node\Scalar\MagicConst\Namespace_();
                default:
                    throw new \Exception('Invlaid magic constant type: ' . $this->type);
            }
        }
    }

    class Literal extends Expr {
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

        /** @var array|bool|float|int|null|string */
        private $value;

        /**
         * @param string|int|float|bool|null|array $value
         */
        public function __construct($value) {
            $this->value = $value;
        }

        public function subStmts():array {
            return [];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return self::literalToNode($this->value);
        }

        public function unparseOrString() {
            $value = $this->value;
            if (is_string($value)) {
                return $value;
            } else {
                return parent::unparseOrString();
            }
        }

        public function unparseOrName() {
            if (is_string($this->value)) {
                return new \PhpParser\Node\Name\FullyQualified($this->value);
            } else {
                return parent::unparseOrName();
            }
        }
    }

    abstract class Call extends Expr {
        /** @var CallArg[] */
        private $args = [];

        /**
         * @param CallArg[] $args
         */
        public function __construct(array $args) {
            $this->args = $args;
        }

        public function subStmts():array {
            $stmts = [];
            foreach ($this->args as $arg) {
                $stmts[] = $arg->expr();
            }
            return $stmts;
        }

        protected function unparseArgs():array {
            $args = [];
            foreach ($this->args as $arg) {
                $args[] = $arg->unparse();
            }
            return $args;
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

        public function subStmts():array {
            return array_merge(parent::subStmts(), [$this->function]);
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\FuncCall(
                $this->function->unparseOrName(),
                $this->unparseArgs()
            );
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

        public function subStmts():array {
            return array_merge(parent::subStmts(), [$this->class, $this->method]);
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\StaticCall(
                $this->class->unparseOrName(),
                $this->method->unparseOrString(),
                $this->unparseArgs()
            );
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

        public function subStmts():array {
            return array_merge(parent::subStmts(), [$this->object, $this->method]);
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\MethodCall(
                $this->object->unparse_(),
                $this->method->unparseOrString(),
                $this->unparseArgs()
            );
        }
    }

    class CallArg extends Node {
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

        public function expr() {
            return $this->expr;
        }

        public function unparse():\PhpParser\Node\Arg {
            return new \PhpParser\Node\Arg(
                $this->expr->unparse_(),
                $this->byRef,
                $this->splat
            );
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

        public function subStmts():array {
            $stmts = [];
            foreach ($this->items as $item) {
                foreach ($item->subStmts() as $stmt) {
                    $stmts[] = $stmt;
                }
            }
            return $stmts;
        }

        public function unparse_():\PhpParser\Node\Expr {
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
         * @param Expr|null $key
         * @param Expr      $value
         * @param bool      $byRef
         */
        public function __construct(Expr $key = null, Expr $value, $byRef) {
            $this->key   = $key;
            $this->value = $value;
            $this->byRef = $byRef;
        }

        public function subStmts() {
            $stmts = [$this->value];
            if ($this->key) {
                $stmts[] = $this->key;
            }
            return $stmts;
        }

        public function unparse():\PhpParser\Node\Expr\ArrayItem {
            return new \PhpParser\Node\Expr\ArrayItem(
                $this->value->unparse_(),
                $this->key ? $this->key->unparse_() : null,
                $this->byRef
            );
        }
    }

    class New_ extends Call {
        /** @var Expr */
        private $class;

        /**
         * @param Expr      $class
         * @param CallArg[] $args
         */
        public function __construct(Expr $class, array $args) {
            $this->class = $class;
            parent::__construct($args);
        }

        public function subStmts():array {
            $stmts   = parent::subStmts();
            $stmts[] = $this->class;
            return $stmts;
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\New_(
                $this->class->unparseOrName(),
                $this->unparseArgs()
            );
        }
    }

    class Print_ extends Expr {
        /** @var Expr */
        private $expr;

        public function __construct(Expr $expr) {
            $this->expr = $expr;
        }

        public function subStmts():array {
            return [$this->expr];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\Print_($this->expr->unparse_());
        }
    }

    class Closure extends Expr {
        /** @var bool */
        private $static;
        /** @var Stmt\FunctionSignature */
        private $type;
        /** @var ClosureUse[] */
        private $uses;
        /** @var Stmt\Stmt */
        private $body;

        /**
         * @param bool                   $static
         * @param Stmt\FunctionSignature $type
         * @param ClosureUse[]           $uses
         * @param Stmt\Stmt              $body
         */
        public function __construct($static, Stmt\FunctionSignature $type, array $uses, Stmt\Stmt $body) {
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

        public function unparse_():\PhpParser\Node\Expr {
            $subNodes = $this->type->unparseAttributes();

            $uses = [];
            foreach ($this->uses as $use) {
                $uses[] = $use->unparse();
            }

            return new \PhpParser\Node\Expr\Closure(array_replace($subNodes, [
                'static' => $this->static,
                'uses'   => $uses,
                'stmts'  => $this->body->unparse(),
            ]));
        }
    }

    class ClosureUse extends Node {
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

        public function subStmts():array {
            $stmts = [$this->cond, $this->false];
            if ($this->true) {
                $stmts[] = $this->true;
            }
            return $stmts;
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\Ternary(
                $this->cond->unparse_(),
                $this->true ? $this->true->unparse_() : null,
                $this->false->unparse_()
            );
        }
    }

    class ConcatMany extends Expr {
        /**
         * @param Expr[] $exprs
         * @return \PhpParser\Node\Expr[]
         */
        public static function unparseEncaps($exprs) {
            $parts = [];
            foreach ($exprs as $expr) {
                $expr = $expr->unparseOrString();
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
         * @param Expr[] $exprs
         */
        public function __construct(array $exprs) {
            $this->exprs = $exprs;
        }

        public function subStmts():array {
            return $this->exprs;
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Scalar\Encapsed(self::unparseEncaps($this->exprs));
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

        public function subStmts():array {
            return $this->exprs;
        }

        public function unparse_():\PhpParser\Node\Expr {
            $exprs = [];
            foreach ($this->exprs as $expr) {
                $exprs[] = $expr->unparse_();
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

        public function subStmts():array {
            return [$this->left, $this->right];
        }

        public function unparse_():\PhpParser\Node\Expr {
            switch ($this->type) {
                case self:: INSTANCEOF:
                    return new \PhpParser\Node\Expr\Instanceof_(
                        $this->left->unparse_(),
                        $this->right->unparseOrName()
                    );
            }
            $left  = $this->left->unparse_();
            $right = $this->right->unparse_();
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

        /**
         * @param string $type
         * @param Expr   $expr
         */
        public function __construct($type, Expr $expr) {
            $this->type = $type;
            $this->expr = $expr;
        }

        public function subStmts():array {
            return [$this->expr];
        }

        public function unparse_():\PhpParser\Node\Expr {
            $expr = $this->expr->unparse_();
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

        /**
         * @param string $type
         * @param Expr   $expr
         */
        public function __construct($type, Expr $expr) {
            $this->type = $type;
            $this->expr = $expr;
        }

        public function subStmts():array {
            return [$this->expr];
        }

        public function unparse_():\PhpParser\Node\Expr {
            $expr = $this->expr->unparse_();
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

        /**
         * @param Expr|null $expr
         */
        public function __construct(Expr $expr = null) {
            $this->expr = $expr;
        }

        public function subStmts():array {
            return $this->expr ? [$this->expr] : [];
        }

        public function unparse_():\PhpParser\Node\Expr {
            $expr = $this->expr ? $this->expr->unparse_() : null;
            return new \PhpParser\Node\Expr\Exit_($expr);
        }
    }

    class ShellExec extends Expr {
        /** @var Expr[] */
        private $parts;

        /**
         * @param Expr[] $parts
         */
        public function __construct(array $parts) {
            $this->parts = $parts;
        }

        public function subStmts():array {
            return $this->parts;
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\ShellExec(ConcatMany::unparseEncaps($this->parts));
        }
    }

    abstract class AbstractClassName extends Expr {
        /**
         * @param string|null $static
         * @return string
         */
        public abstract function toString($static = null);
    }

    /**
     * Foo\Bar::class
     */
    class ClassName extends AbstractClassName {
        /** @var string */
        private $class;

        /**
         * @param string $class
         * @throws \Exception
         */
        public function __construct($class) {
            if (substr($class, 0, 1) === '\\') {
                throw new \Exception("Illegal class name: $class");
            }
            $this->class = $class;
        }

        public function toString($static = null) {
            return $this->class;
        }

        public function subStmts():array {
            return [];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\ClassConstFetch(new \PhpParser\Node\Name\FullyQualified($this->class), 'class');
        }

        public function unparseOrName() {
            return new \PhpParser\Node\Name\FullyQualified($this->class);
        }

        public function unparseOrString() {
            return $this->class;
        }
    }

    /**
     * static::name
     */
    class StaticClassName extends AbstractClassName {
        public function toString($static = null) {
            if ($static === null) {
                throw new \Exception('"static" used in disallowed context');
            } else {
                return $static;
            }
        }

        public function subStmts():array {
            return [];
        }

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\ClassConstFetch(
                new \PhpParser\Node\Name('static'),
                'class'
            );
        }

        public function unparseOrName() {
            return new \PhpParser\Node\Name('static');
        }
    }
}

namespace JesseSchalken\PhpTypeChecker\Node\Type {

    use JesseSchalken\PhpTypeChecker\Node\Node;

    abstract class Type extends Node {
        /**
         * @return null|string|\PhpParser\Node\Name
         */
        public abstract function unparse();

        /**
         * @return string
         */
        public abstract function toString();
    }

    class Mixed extends Type {
        public function unparse() {
            return null;
        }

        public function toString() {
            return 'mixed';
        }
    }

    class SimpleType extends Type {
        const INT      = 'int';
        const STRING   = 'string';
        const FLOAT    = 'float';
        const NULL     = 'null';
        const OBJECT   = 'object';
        const ARRAY    = 'array';
        const RESOURCE = 'resource';
        const BOOL     = 'bool';

        /** @var string */
        private $type;

        /**
         * @param string $type
         */
        public function __construct($type) {
            $this->type = $type;
        }

        public function unparse() {
            switch ($this->type) {
                case self::ARRAY:
                    return 'array';
                case self::BOOL:
                    return 'bool';
                case self::FLOAT:
                    return 'float';
                case self::INT:
                    return 'int';
                case self::STRING:
                    return 'string';
                default:
                    return null;
            }
        }

        public function toString() {
            return $this->type;
        }
    }

    class Callable_ extends Type {
        public function unparse() {
            return 'callable';
        }

        public function toString() {
            return 'callable';
        }
    }

    class ObjectType extends Type {
        /** @var string */
        private $class;

        /**
         * @param string $class
         */
        public function __construct($class) {
            $this->class = $class;
        }

        public function unparse() {
            return new \PhpParser\Node\Name\FullyQualified($this->class);
        }

        public function toString() {
            return $this->class;
        }
    }
}
