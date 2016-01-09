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

        public function __construct(string $path, int $line, int $column) {
            $this->path   = $path;
            $this->line   = $line;
            $this->column = $column;
        }

        public function format(string $message):string {
            return "$this->path($this->line,$this->column): $message\n";
        }

        public function toDocBlockLocation() {
            return new \phpDocumentor\Reflection\DocBlock\Location($this->line, $this->column);
        }
    }

    class ErrorReceiver {
        public function add($message, CodeLoc $loc) {
            print $loc->format($message);
        }
    }

    abstract class Node {
        /** @var CodeLoc */
        private $loc;

        public function __construct(CodeLoc $loc) {
            $this->loc = $loc;
        }

        public final function loc() {
            return $this->loc;
        }
    }

    class File extends Node {
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
                $self           = new self($file->nullLoc());
                $self->path     = $file->path;
                $self->shebang  = $file->shebang;
                $self->contents = (new Parser\Parser($file, $defined, $errors))->parseStmts($self->loc(), $file->nodes);
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

        public function path():string {
            return $this->path;
        }

        public function unparse():string {
            return $this->shebang . (new \PhpParser\PrettyPrinter\Standard())->prettyPrintFile($this->contents->unparseWithNamespaces());
        }
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

    abstract class Uses {
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

        public final function resolve(\PhpParser\Node\Name $name, Uses $classes):\PhpParser\Node\Name {
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

        public final function add(\PhpParser\Node\Name $name, $alias = null) {
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

    class ClassUses extends Uses {
        protected function normalize(string $alias):string {
            return strtolower($alias);
        }
    }

    class FunctionUses extends Uses {
        protected function normalize(string $alias):string {
            return strtolower($alias);
        }

        protected function resolveDefault(string $alias):\PhpParser\Node\Name {
            $local = parent::resolveDefault($alias);

            return $this->defined($local) ? $local : new \PhpParser\Node\Name([$alias]);
        }
    }

    class ConstantUses extends Uses {
        protected function resolveDefault(string $alias):\PhpParser\Node\Name {
            $local = parent::resolveDefault($alias);

            return $this->defined($local) ? $local : new \PhpParser\Node\Name([$alias]);
        }
    }

    class DefinedNamesConstants extends DefinedNames {
        protected function normalize(string $name):string {
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
        protected function normalize(string $name):string {
            return strtolower($name);
        }
    }

    class DefinedNamesCaseSensitive extends DefinedNames {
        protected function normalize(string $name):string {
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

        public function __construct(string $path, ErrorReceiver $errors) {
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

    class Parser {
        const TEMPORARY_TRAIT_CLASS = '__TEMPORARY_TRAIT_CLASS';

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
        /** @var ErrorReceiver */
        private $errors;

        public function __construct(ParsedFile $file, GlobalDefinedNames $globals, ErrorReceiver $errors) {
            $this->globals     = $globals;
            $this->locals      = new DefinedNamesCaseSensitive();
            $this->finallys[0] = new Stmt\Block($file->nullLoc());
            $this->file        = $file;
            $this->errors      = $errors;
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

            return new Expr\ClassName($loc, $this->useClass->resolve($name, $this->useClass)->toString());
        }

        private function resolveConst(\PhpParser\Node\Name $name):string {
            return $this->useConstant->resolve($name, $this->useClass)->toString();
        }

        private function resolveFunction(\PhpParser\Node\Name $name):string {
            return $this->useFunction->resolve($name, $this->useClass)->toString();
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
                $stmts->add($this->parseStmt($node));
            }
            return $stmts;
        }

        private function newUnusedvariable():string {
            return $this->locals->create('_');
        }

        private function getFinally():Stmt\Block {
            return new Stmt\Block($this->nullLoc(), $this->finallys);
        }

        private function nullLoc():CodeLoc {
            return new CodeLoc($this->file->path, 1, 1);
        }

        private function hasFinally():bool {
            return $this->getFinally()->split() ? true : false;
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
                    $false = new Stmt\If_(
                        $loc1,
                        $this->parseExpr($elseIf->cond),
                        $this->parseStmts($loc1, $elseIf->stmts),
                        $false
                    );
                }

                return new Stmt\If_(
                    $loc,
                    $this->parseExpr($node->cond),
                    $this->parseStmts($loc, $node->stmts),
                    $false
                );
            } elseif ($node instanceof \PhpParser\Node\Stmt\Return_) {
                $expr = $this->parseExprNull($node->expr);

                if ($this->hasFinally()) {
                    $stmts = [];
                    if ($expr) {
                        $var     = new Expr\Variable($loc, new Expr\Literal($loc, $this->newUnusedvariable()));
                        $stmts[] = new Expr\BinOp(
                            $loc,
                            $var,
                            $this->returnRef ? Expr\BinOp::ASSIGN_REF : Expr\BinOp::ASSIGN,
                            $expr
                        );
                        $expr    = $var;
                    }
                    return new Stmt\Block($loc, array_merge(
                        $stmts,
                        $this->finallys,
                        [new Stmt\Return_($loc, $expr)]
                    ));
                }

                return new Stmt\Return_($loc, $expr);
            } elseif ($node instanceof \PhpParser\Node\Stmt\Namespace_) {
                $copy = clone $this;
                $copy->resetNamespace($node->name);
                return $copy->parseStmts($loc, $node->stmts);
            } elseif ($node instanceof \PhpParser\Node\Stmt\Class_) {
                return $this->parseClass($node);
            } elseif ($node instanceof \PhpParser\Node\Stmt\Function_) {
                $name = $this->prefixName($node->name);
                $self = clone $this;
                $self->resetMagicConstants();
                $self->function = $name;
                return new Stmt\Function_(
                    $loc,
                    $name,
                    $self->parseFunctionType($node),
                    $self->parseStmts($loc, $node->stmts)
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
                return new Stmt\Interface_($loc, $name, $extends, $self->parseClassMembers($node));
            } elseif ($node instanceof \PhpParser\Node\Stmt\Trait_) {
                $name = $this->prefixName($node->name);
                $self = clone $this;

                $self->resetMagicConstants();
                $self->trait  = $name;
                $self->class  = self::TEMPORARY_TRAIT_CLASS;
                $self->parent = self::TEMPORARY_TRAIT_CLASS;
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

                return new Stmt\Block($loc, array_merge(
                    array_slice($this->finallys, 0, $levels),
                    [new Stmt\Break_($loc, $levels)]
                ));
            } elseif ($node instanceof \PhpParser\Node\Stmt\Continue_) {
                if ($node->num === null) {
                    $levels = 1;
                } elseif ($node->num instanceof \PhpParser\Node\Scalar\LNumber) {
                    $levels = $node->num->value;
                } else {
                    throw new \Exception('"continue" statement must use a constant operand');
                }

                return new Stmt\Block($loc, array_merge(
                    array_slice($this->finallys, 0, $levels),
                    [new Stmt\Continue_($loc, $levels)]
                ));
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
                if ($node->finallyStmts) {
                    $finally      = $this->parseStmts($loc, $node->finallyStmts);
                    $finallyVar   = new Expr\Variable($loc, new Expr\Literal($loc, $this->newUnusedvariable()));
                    $exceptionVar = new Expr\Variable($loc, new Expr\Literal($loc, $excVar = $this->newUnusedvariable()));

                    $self = clone $this;
                    $self->finallys[0]->add(new Stmt\Block($loc, [
                        new Expr\BinOp($loc, $finallyVar, Expr\BinOp::ASSIGN, new Expr\Literal($loc, true)),
                        $finally,
                    ]));

                    return new Stmt\Block($loc, [
                        new Expr\BinOp($loc, $finallyVar, Expr\BinOp::ASSIGN, new Expr\Literal($loc, false)),
                        new Stmt\Try_(
                            $loc,
                            $self->parseTryCatch($node),
                            [new Stmt\Catch_($loc, 'Exception', $excVar, new Stmt\Block($loc))]
                        ),
                        new Stmt\If_(
                            $loc,
                            new Expr\UnOp($loc, Expr\UnOp::BOOL_NOT, $finallyVar),
                            $finally,
                            new Stmt\Block($loc)
                        ),
                        new Stmt\If_(
                            $loc,
                            new Expr\Isset_($loc, [$exceptionVar]),
                            new Stmt\Throw_($loc, $exceptionVar),
                            new Stmt\Block($loc)
                        ),
                    ]);
                } else {
                    return $this->parseTryCatch($node);
                }
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
                    $stmts[] = new Stmt\Method_(
                        $loc,
                        $stmt->name,
                        $this->parseFunctionType($stmt),
                        $stmt->stmts === null ? null : $this->parseStmts($loc, $stmt->stmts),
                        !!($stmt->type & \PhpParser\Node\Stmt\Class_::MODIFIER_FINAL),
                        $this->parseVisibility($stmt->type),
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
                        $comment = $this->parseDocBlock($prop->getDocComment() ?: $stmt->getDocComment());
                        $type    = null;
                        if ($comment) {
                            /** @var \phpDocumentor\Reflection\DocBlock\Tag\VarTag $tag */
                            foreach ($comment->getTagsByName('var') as $tag) {
                                $type = $type ?: $this->parseDocType($this->locateComment($comment), $tag->getType(), $comment->getContext());
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
                            $useTrait->addInsteadOf(new Stmt\UseTraitInsteadof(
                                $this->locateNode($adaption),
                                $this->resolveClass($adaption->trait)->toString(),
                                $adaption->method,
                                $insteadOf
                            ));
                        } elseif ($adaption instanceof \PhpParser\Node\Stmt\TraitUseAdaptation\Alias) {
                            $useTrait->addAlias(new Stmt\UseTraitAlias(
                                $this->locateNode($adaption),
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
        private function parseDocBlock(\PhpParser\Comment\Doc $comment = null) {
            if (!$comment) {
                return null;
            }

            return new \phpDocumentor\Reflection\DocBlock(
                $comment->getText(),
                $this->getDocBlockContext(),
                new \phpDocumentor\Reflection\DocBlock\Location($comment->getLine(), 1)
            );
        }

        private function getDocBlockContext():\phpDocumentor\Reflection\DocBlock\Context {
            return new \phpDocumentor\Reflection\DocBlock\Context(
                $this->namespace->toString(),
                $this->useClass->getMap()
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
            $context = new \phpDocumentor\Reflection\Types\Context($context->getNamespace(), $context->getNamespaceAliases());
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
                    return new Type\Object($loc, trim($fqsen->__toString(), '\\'));
                } else {
                    return new Type\Object($loc);
                }
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Array_) {
                $inner = $type->getValueType();
                if ($inner instanceof \phpDocumentor\Reflection\Types\Mixed) {
                    return new Type\Array_($loc, new Type\Mixed($loc));
                } else {
                    return new Type\Array_($loc, $this->parseDocTypeObject($loc, $inner, $context));
                }
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Null_) {
                return new Type\Null_($loc);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Void) {
                return new Type\Null_($loc);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Static_) {
                return new Type\Static_($loc);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Resource) {
                return new Type\Resource($loc);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Mixed) {
                return new Type\Mixed($loc);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Scalar) {
                return new Type\Scalar($loc);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\This) {
                return new Type\This($loc);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Boolean) {
                return new Type\Bool_($loc);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Callable_) {
                return new Type\Callable_($loc);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Compound) {
                $types = [];
                for ($i = 0; $type->has($i); $i++) {
                    $types[] = $this->parseDocTypeObject($loc, $type->get($i), $context);
                }
                return new Type\Union($loc, $types);
            } else if ($type instanceof \phpDocumentor\Reflection\Types\Self_) {
                if (!$this->class) {
                    throw new \Exception('Use of "self" outside a class');
                }
                return new Type\Object($loc, $this->class);
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
            if (!$sup->triviallyContains($sub)) {
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
            $paramTypes = [];
            $paramDefaults = [];
            foreach ($node->getParams() as $param) {
                $name    = $param->name;
                $type    = $this->parseType($this->locateNode($param), $param->type);
                $default = $this->parseExprNull($param->default);

                if ($default && $default instanceof Expr\Literal && $default->value() === null) {
                    $type = new Type\Union($loc, [$type, new Type\Null_($default->loc())]);
                }

                $paramDefaults[$name] = $default;
                $paramTypes[$name]    = $type;
            }
            $returnType = $this->parseType($this->locateNode($node), $node->getReturnType());

            // If we have a doc comment, overwrite the types from the type hints with those
            // from the doc comment, but make sure they are trivially compatible.
            if ($comment = $this->parseDocBlock($node->getDocComment())) {
                /** @var \phpDocumentor\Reflection\DocBlock\Tag\ReturnTag $tag */
                foreach ($comment->getTagsByName('return') as $tag) {
                    if ($type = $this->parseDocType($this->locateComment($comment), $tag->getType(), $comment->getContext())) {
                        $this->checkCompatible($returnType, $type);
                        $returnType = $type;
                    }
                }
                /** @var \phpDocumentor\Reflection\DocBlock\Tag\ParamTag $tag */
                foreach ($comment->getTagsByName('param') as $tag) {
                    $name = substr($tag->getVariableName(), 1);
                    if ($type = $this->parseDocType($this->locateComment($comment), $tag->getType(), $comment->getContext())) {
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
                    case 'string':
                        return new Type\String_($loc);
                    case 'double':
                    case 'float':
                        return new Type\Float_($loc);
                    case 'bool':
                    case 'boolean':
                        return new Type\Bool_($loc);
                    case 'null':
                    case 'void':
                        return new Type\Null_($loc);
                    case 'object':
                        return new Type\Object($loc);
                    case 'resource':
                        return new Type\Resource($loc);
                    case 'array':
                        return new Type\Array_($loc, new Type\Mixed($loc));
                    case 'callable':
                        return new Type\Callable_($loc);
                    default:
                        throw new \Exception('Invalid simple type: ' . $type);
                }
            } else if ($type instanceof \PhpParser\Node\Name) {
                return new Type\Object($loc, $this->resolveClass($type)->toString());
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
         * @param CodeLoc                     $loc
         * @return Expr\Expr
         * @throws \Exception
         */
        private function parseExprString($node, CodeLoc $loc):Expr\Expr {
            return is_string($node) ? new Expr\Literal($loc, $node) : $this->parseExpr($node);
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
                return new Expr\Variable($loc, $this->parseExprString($node->name, $loc));
            } elseif ($node instanceof \PhpParser\Node\Expr\ConstFetch) {
                $name = $this->resolveConst($node->name);
                switch (strtolower($name)) {
                    case 'true':
                        return new Expr\Literal($loc, true);
                    case 'false':
                        return new Expr\Literal($loc, false);
                    case 'null':
                        return new Expr\Literal($loc, null);
                    default:
                        return new Expr\ConstFetch($loc, $name);
                }
            } elseif ($node instanceof \PhpParser\Node\Expr\Assign) {
                return new Expr\BinOp(
                    $loc,
                    $this->parseExpr($node->var),
                    Expr\BinOp::ASSIGN,
                    $this->parseExpr($node->expr)
                );
            } elseif ($node instanceof \PhpParser\Node\Scalar\LNumber) {
                return new Expr\Literal($loc, $node->value);
            } elseif ($node instanceof \PhpParser\Node\Scalar\DNumber) {
                return new Expr\Literal($loc, $node->value);
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
                $type = $node->getName();
                $line = $node->getAttribute('startLine');
                return new Expr\MagicConst($loc, $type, $this->getMagicConstValue($type, $line));
            } elseif ($node instanceof \PhpParser\Node\Scalar\String_) {
                return new Expr\Literal($loc, $node->value);
            } elseif ($node instanceof \PhpParser\Node\Expr\StaticCall) {
                return new Expr\StaticCall(
                    $loc,
                    $this->parseArgs($node->args),
                    $this->parseExprClass($node->class),
                    $this->parseExprString($node->name, $loc)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\FuncCall) {
                $function = $node->name;
                $function = $function instanceof \PhpParser\Node\Name
                    ? new Expr\Literal($this->locateNode($function), $this->resolveFunction($function))
                    : $this->parseExpr($function);

                return new Expr\FunctionCall($loc, $function, $this->parseArgs($node->args));
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
                return new Expr\ArrayAccess(
                    $loc,
                    $this->parseExpr($node->var),
                    $this->parseExprNull($node->dim)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\MethodCall) {
                return new Expr\MethodCall(
                    $loc,
                    $this->parseArgs($node->args),
                    $this->parseExpr($node->var),
                    $this->parseExprString($node->name, $loc)
                );
            } elseif ($node instanceof \PhpParser\Node\Expr\New_) {
                $class = $node->class;
                if ($class instanceof \PhpParser\Node\Stmt\Class_) {
                    $class   = $this->parseClass($class, $this->globals->classes->create($this->prefixName('class')));
                    $stmts[] = $class;
                    $class   = new Expr\Literal($class->loc(), $class->name());
                } else {
                    $class = $this->parseExprClass($class);
                }

                return new Expr\New_($loc, $class, $this->parseArgs($node->args));
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
                return new Expr\Literal($loc, $node->value);
            } elseif ($node instanceof \PhpParser\Node\Scalar\Encapsed) {
                $exprs = [];
                foreach ($node->parts as $part) {
                    $exprs[] = $this->parseExpr($part);
                }
                return new Expr\ConcatMany($loc, $exprs);
            } elseif ($node instanceof \PhpParser\Node\Expr\StaticPropertyFetch) {
                $class = $this->parseExprClass($node->class);
                $prop  = $this->parseExprString($node->name, $loc);
                return new Expr\StaticPropertyAccess($loc, $class, $prop);
            } elseif ($node instanceof \PhpParser\Node\Expr\Isset_) {
                return new Expr\Isset_($loc, $this->parseExprs($node->vars));
            } elseif ($node instanceof \PhpParser\Node\Expr\BinaryOp) {
                return $this->parseBinaryOp($node, $loc);
            } elseif ($node instanceof \PhpParser\Node\Expr\AssignOp) {
                return $this->parseAssignOp($node, $loc);
            } elseif ($node instanceof \PhpParser\Node\Expr\ErrorSuppress) {
                return new Expr\UnOp($loc, Expr\UnOp::SUPPRESS, $this->parseExpr($node->expr));
            } elseif ($node instanceof \PhpParser\Node\Expr\PropertyFetch) {
                return new Expr\PropertyAccess(
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
                    return new Expr\ClassConstFetch($loc, $this->parseExprClass($node->class), $node->name);
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
                return new Expr\List_($loc, $exprs);
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

        private function getMagicConstValue(string $type, int $line):string {
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
        private function parseArgs(array $args):array {
            $result = [];
            foreach ($args as $arg) {
                $result[] = new Expr\CallArg(
                    $this->locateNode($arg),
                    $this->parseExpr($arg->value),
                    $arg->byRef,
                    $arg->unpack
                );
            }
            return $result;
        }

        private function parseTryCatch(\PhpParser\Node\Stmt\TryCatch $node):Stmt\Stmt {
            $loc    = $this->locateNode($node);
            $result = $this->parseStmts($loc, $node->stmts);

            if ($node->catches) {
                $catches = [];
                foreach ($node->catches as $catch) {
                    $catches[] = new Stmt\Catch_(
                        $this->locateNode($catch),
                        $this->resolveClass($catch->type)->toString(),
                        $catch->var,
                        $this->parseStmts($this->locateNode($catch), $catch->stmts)
                    );
                }
                $result = new Stmt\Try_($loc, $result, $catches);
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

            $self = clone $this;
            $self->resetMagicConstants();
            $self->class  = $name;
            $self->parent = $parent;
            return new Stmt\Class_(
                $loc,
                $name,
                $self->parseClassMembers($node),
                $parent,
                $implements,
                $node->type & \PhpParser\Node\Stmt\Class_::MODIFIER_ABSTRACT ? true : false,
                $node->type & \PhpParser\Node\Stmt\Class_::MODIFIER_FINAL ? true : false
            );
        }

        private function locateNode(\PhpParser\Node $node):CodeLoc {
            return $this->file->locateNode($node);
        }
    }
}

namespace JesseSchalken\PhpTypeChecker\Node\Stmt {

    use JesseSchalken\PhpTypeChecker\Node\CodeLoc;
    use JesseSchalken\PhpTypeChecker\Node\Expr;
    use JesseSchalken\PhpTypeChecker\Node\Node;
    use JesseSchalken\PhpTypeChecker\Node\Type;
    use JesseSchalken\PhpTypeChecker\Parser;
    use function JesseSchalken\MagicUtils\clone_ref;
    use function JesseSchalken\PhpTypeChecker\recursive_scan2;

    abstract class Stmt extends Node {
        /**
         * @return \PhpParser\Node[]
         * @throws \Exception
         */
        public final function unparseWithNamespaces():array {
            $nodes = [];

            $currentNamespace = null;
            $currentNodes     = [];

            foreach ($this->split() as $stmt) {
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

            return $nodes;
        }

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
         * @param CodeLoc $loc
         * @param Stmt[]  $stmts
         */
        public function __construct(CodeLoc $loc, array $stmts = []) {
            parent::__construct($loc);
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
         * @param CodeLoc   $loc
         * @param Stmt      $body
         * @param Expr\Expr $cond
         */
        public function __construct(CodeLoc $loc, Stmt $body, Expr\Expr $cond) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, Expr\Expr $cond, Stmt $true, Stmt $false) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, Expr\Expr $expr = null) {
            parent::__construct($loc);
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

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\InlineHTML($this->html)];
        }
    }

    abstract class Classish extends SingleStmt {
        private $name;

        public function __construct(CodeLoc $loc, string $name) {
            parent::__construct($loc);
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

        public function makeAnonymous():Expr\Expr {
//            return new If_(
//                new Expr\UnOp(Expr\UnOp::BOOL_NOT, new Expr\FunctionCall(new Expr\Literal('class_exists'), [
//                    new Expr\CallArg(new Expr\Literal($this->name()), false, false),
//                    new Expr\CallArg(new Expr\Literal(false), false, false),
//                ])),
//                new Block(, [$this]),
//                new Block()
//            );
        }

        public function members():array {
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

    function extract_namespace(string $name):string {
        $pos = strrpos($name, '\\');
        return $pos === false ? '' : substr($name, 0, $pos);
    }

    function remove_namespace(string $name):string {
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

        public function __construct(CodeLoc $loc, string $name, FunctionSignature $type, Stmt $body = null) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, string $name, Expr\Expr $value) {
            parent::__construct($loc);
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
        public function __construct(CodeLoc $loc, string $name, Type\Type $type, Expr\Expr $default = null, string $visibility, bool $static) {
            parent::__construct($loc, $visibility, $static);
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
        /** @var Stmt|null */
        private $body;
        /** @var bool */
        private $final;

        /**
         * @param CodeLoc $loc
         * @param string $name
         * @param FunctionSignature $type
         * @param Stmt|null $body
         * @param bool $final
         * @param string $visibility
         * @param bool $static
         */
        public function __construct(
            CodeLoc $loc,
            string $name,
            FunctionSignature $type,
            Stmt $body = null,
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
         * @param CodeLoc $loc
         * @param string $name
         * @param Expr\Expr|null $default
         * @param bool $passByRef
         * @param bool $variadic
         * @param Type\Type $type
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
                $this->default ? $this->default->unparse_() : null,
                $this->type->toTypeHint(),
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
         * @param CodeLoc        $loc
         * @param Expr\Expr      $array
         * @param Expr\Expr|null $key
         * @param Expr\Expr      $value
         * @param bool           $byRef
         * @param Stmt           $body
         */
        public function __construct(
            CodeLoc $loc,
            Expr\Expr $array,
            Expr\Expr $key = null,
            Expr\Expr $value,
            bool $byRef,
            Stmt $body
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
         * @param CodeLoc   $loc
         * @param string    $name
         * @param Expr\Expr $value
         */
        public function __construct(CodeLoc $loc, string $name, Expr\Expr $value) {
            parent::__construct($loc);
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

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Static_([
                new \PhpParser\Node\Stmt\StaticVar(
                    $this->name,
                    $this->value ? $this->value->unparse_() : null
                ),
            ])];
        }
    }

    /**
     * Special statements used for the init/cond/loop parts of a for loop.
     * Is like the comma operator in C and JavaScript but can actually only
     * be used in the head of a for loop.
     */
    class Comma extends SingleStmt {
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
        public function unparse():array {
            $nodes = [];
            foreach ($this->exprs as $expr) {
                $nodes[] = $expr->unparse_();
            }
            return $nodes;
        }
    }

    class For_ extends SingleStmt {
        /** @var Comma */
        private $init;
        /** @var Comma */
        private $cond;
        /** @var Comma */
        private $loop;
        /** @var Stmt */
        private $body;

        public function __construct(CodeLoc $loc, Comma $init, Comma $cond, Comma $loop, Stmt $body) {
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

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\For_([
                'init'  => $this->init->unparse(),
                'cond'  => $this->cond->unparse(),
                'loop'  => $this->loop->unparse(),
                'stmts' => $this->body->unparse(),
            ])];
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
         * @param CodeLoc        $loc
         * @param Expr\Expr|null $expr
         * @param Stmt           $stmt
         */
        public function __construct(CodeLoc $loc, Expr\Expr $expr = null, Stmt $stmt) {
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
                $this->expr ? $this->expr->unparse_() : null,
                $this->stmt->unparse()
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
         * @param CodeLoc   $loc
         * @param Expr\Expr $cond
         * @param Stmt      $body
         */
        public function __construct(CodeLoc $loc, Expr\Expr $cond, Stmt $body) {
            parent::__construct($loc);
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
         * @param CodeLoc  $loc
         * @param Stmt     $body
         * @param Catch_[] $catches
         */
        public function __construct(CodeLoc $loc, Stmt $body, array $catches) {
            parent::__construct($loc);
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
         * @param CodeLoc $loc
         * @param string  $class
         * @param string  $variable
         * @param Stmt    $body
         */
        public function __construct(CodeLoc $loc, string $class, string $variable, Stmt $body) {
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
                $this->body->unparse()
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

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Global_([
                $this->expr->unparse_(),
            ])];
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

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Label($this->name)];
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

        public function unparse():array {
            return [new \PhpParser\Node\Stmt\Goto_($this->name)];
        }
    }
}

namespace JesseSchalken\PhpTypeChecker\Node\Expr {

    use JesseSchalken\PhpTypeChecker\Node\CodeLoc;
    use JesseSchalken\PhpTypeChecker\Node\Node;
    use JesseSchalken\PhpTypeChecker\Node\Stmt;

    abstract class Expr extends Stmt\SingleStmt {
        public function isLValue():bool {
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
         * @param CodeLoc $loc
         * @param (Expr|null)[]   $exprs
         */
        public function __construct(CodeLoc $loc, array $exprs) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, Expr $key = null, Expr $value = null) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, string $name) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, Expr $class, string $const) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, Expr $name) {
            parent::__construct($loc);
            $this->name = $name;
        }

        public function isLValue():bool {
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
         * @param CodeLoc $loc
         * @param Expr    $object
         * @param Expr    $property
         */
        public function __construct(CodeLoc $loc, Expr $object, Expr $property) {
            parent::__construct($loc);
            $this->object   = $object;
            $this->property = $property;
        }

        public function isLValue():bool {
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

        public function __construct(CodeLoc $loc, Expr $class, Expr $property) {
            parent::__construct($loc);
            $this->class    = $class;
            $this->property = $property;
        }

        public function isLValue():bool {
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
         * @param CodeLoc   $loc
         * @param Expr      $array
         * @param Expr|null $key
         */
        public function __construct(CodeLoc $loc, Expr $array, Expr $key = null) {
            parent::__construct($loc);
            $this->array = $array;
            $this->key   = $key;
        }

        public function isLValue():bool {
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
         * @param CodeLoc    $loc
         * @param string     $type
         * @param int|string $value
         */
        public function __construct(CodeLoc $loc, string $type, $value) {
            parent::__construct($loc);
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
         * @param CodeLoc                          $loc
         * @param string|int|float|bool|null|array $value
         */
        public function __construct(CodeLoc $loc, $value) {
            parent::__construct($loc);
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

        public function value() {
            return $this->value;
        }
    }

    abstract class Call extends Expr {
        /** @var CallArg[] */
        private $args = [];

        /**
         * @param CodeLoc   $loc
         * @param CallArg[] $args
         */
        public function __construct(CodeLoc $loc, array $args) {
            parent::__construct($loc);
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
         * @param CodeLoc   $loc
         * @param Expr      $function
         * @param CallArg[] $args
         */
        public function __construct(CodeLoc $loc, Expr $function, array $args) {
            parent::__construct($loc, $args);
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
         * @param CodeLoc   $loc
         * @param CallArg[] $args
         * @param Expr      $class
         * @param Expr      $method
         */
        public function __construct(CodeLoc $loc, array $args, Expr $class, Expr $method) {
            parent::__construct($loc, $args);
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
         * @param CodeLoc   $loc
         * @param CallArg[] $args
         * @param Expr      $object
         * @param Expr      $method
         */
        public function __construct(CodeLoc $loc, array $args, Expr $object, Expr $method) {
            parent::__construct($loc, $args);
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
        /** @var bool */
        private $byRef = false;
        /** @var bool */
        private $splat = false;

        public function __construct(CodeLoc $loc, Expr $expr, bool $byRef, bool $splat) {
            parent::__construct($loc);
            $this->expr  = $expr;
            $this->byRef = $byRef;
            $this->splat = $splat;
        }

        public function expr():Expr {
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
         * @param CodeLoc   $loc
         * @param Expr      $class
         * @param CallArg[] $args
         */
        public function __construct(CodeLoc $loc, Expr $class, array $args) {
            parent::__construct($loc, $args);
            $this->class = $class;
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

        public function __construct(CodeLoc $loc, Expr $expr) {
            parent::__construct($loc);
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
         * @param CodeLoc                $loc
         * @param bool                   $static
         * @param Stmt\FunctionSignature $type
         * @param ClosureUse[]           $uses
         * @param Stmt\Stmt              $body
         */
        public function __construct(CodeLoc $loc, bool $static, Stmt\FunctionSignature $type, array $uses, Stmt\Stmt $body) {
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
        public static function unparseEncaps(array $exprs):array {
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

        public function unparse_():\PhpParser\Node\Expr {
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

        public function __construct(CodeLoc $loc, Expr $left, string $type, Expr $right) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, string $type, Expr $expr) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, string $type, Expr $expr) {
            parent::__construct($loc);
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

        public function __construct(CodeLoc $loc, Expr $expr = null) {
            parent::__construct($loc);
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

        public function unparse_():\PhpParser\Node\Expr {
            return new \PhpParser\Node\Expr\ShellExec(ConcatMany::unparseEncaps($this->parts));
        }
    }

    abstract class AbstractClassName extends Expr {
        public abstract function toString(string $static = null):string;
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

        public function toString(string $static = null):string {
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
        public function toString(string $static = null):string {
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

    use JesseSchalken\PhpTypeChecker\Node\CodeLoc;
    use JesseSchalken\PhpTypeChecker\Node\Node;

    abstract class Type extends Node {
        /**
         * @return null|string|\PhpParser\Node\Name
         */
        public abstract function toTypeHint();

        public abstract function toString(bool $atomic = false):string;

        public final function __toString():string {
            return $this->toString();
        }

        public final function triviallyContains(Type $type):bool {
            foreach ($type->split() as $t) {
                if (!$this->_triviallyContains($t)) {
                    return false;
                }
            }
            return true;
        }

        abstract protected function _triviallyContains(SingleType $type):bool;

        public final function triviallyEquivelant(Type $type):bool {
            return
                $this->triviallyContains($type) &&
                $type->triviallyContains($this);
        }

        public final function isEmpty():bool {
            return count($this->split()) == 0;
        }

        /** @return SingleType[] */
        public abstract function split();

        public final function isMixed():bool {
            return $this->triviallyEquivelant(new Mixed($this->loc()));
        }
    }

    abstract class SingleType extends Type {
        /** @return SingleType[] */
        public final function split():array {
            return [$this];
        }
    }

    class Int_ extends SingleType {
        public function toTypeHint() {
            return 'int';
        }

        public function toString(bool $atomic = false):string {
            return 'int';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self;
        }
    }

    class String_ extends SingleType {
        public function toTypeHint() {
            return 'string';
        }

        public function toString(bool $atomic = false):string {
            return 'string';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self;
        }
    }

    class Null_ extends SingleType {
        public function toTypeHint() {
            return 'null';
        }

        public function toString(bool $atomic = false):string {
            return 'null';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self;
        }
    }

    class Bool_ extends SingleType {
        public function toTypeHint() {
            return 'bool';
        }

        public function toString(bool $atomic = false):string {
            return 'bool';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self;
        }
    }

    class Float_ extends SingleType {
        public function toTypeHint() {
            return 'float';
        }

        public function toString(bool $atomic = false):string {
            return 'float';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self;
        }
    }

    class Resource extends SingleType {
        public function toTypeHint() {
            return null;
        }

        public function toString(bool $atomic = false):string {
            return 'resource';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self;
        }
    }

    class Mixed extends SingleType {
        public function toTypeHint() {
            return null;
        }

        public function toString(bool $atomic = false):string {
            return 'mixed';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return true;
        }
    }

    class Scalar extends SingleType {
        public function toTypeHint() {
            return null;
        }

        public function toString(bool $atomic = false):string {
            return 'scalar';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return
                $type instanceof String_ ||
                $type instanceof Bool_ ||
                $type instanceof Scalar ||
                $type instanceof Int_ ||
                $type instanceof Float_;
        }
    }

    /**
     * Either a:
     * - string representing a global function
     * - object implementing the __invoke() method
     * - array of form [$object, 'method']
     * - array of form ['class', 'method']
     */
    class Callable_ extends SingleType {
        public function toTypeHint() {
            return 'callable';
        }

        public function toString(bool $atomic = false):string {
            return 'callable';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self;
        }
    }

    class Object extends SingleType {
        /** @var string|null */
        private $class;

        public function __construct(CodeLoc $loc, string $class = null) {
            parent::__construct($loc);
            $this->class = $class;
        }

        public function toTypeHint() {
            if ($this->class) {
                return new \PhpParser\Node\Name\FullyQualified($this->class);
            } else {
                return null;
            }
        }

        public function toString(bool $atomic = false):string {
            return $this->class ?: 'object';
        }

        protected function _triviallyContains(SingleType $type):bool {
            if ($this->class === null) {
                return
                    $type instanceof self ||
                    $type instanceof Static_ ||
                    $type instanceof This;
            } else if ($type instanceof self) {
                return strtolower($type->class) == strtolower($this->class);
            } else {
                return false;
            }
        }
    }

    class Static_ extends SingleType {
        public function toTypeHint() {
            return null;
        }

        public function toString(bool $atomic = false):string {
            return 'static';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self;
        }
    }

    class This extends SingleType {
        public function toTypeHint() {
            return null;
        }

        public function toString(bool $atomic = false):string {
            return '$this';
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self;
        }
    }

    class Union extends Type {
        /** @var Type[] */
        private $types = [];

        /**
         * @param CodeLoc $loc
         * @param Type[]  $types
         */
        public function __construct(CodeLoc $loc, array $types) {
            parent::__construct($loc);
            $this->types = $types;
        }

        public function toTypeHint() {
            if (count($this->types) == 1) {
                return $this->types[0]->toTypeHint();
            } else {
                return null;
            }
        }

        public function toString(bool $atomic = false):string {
            switch (count($this->types)) {
                case 0:
                    return '()';
                case 1:
                    return $this->types[0]->toString($atomic);
                default:
                    $types = [];
                    foreach ($this->types as $type) {
                        $types[] = $type->toString(false);
                    }
                    $string = join('|', $types);
                    return $atomic ? "($string)" : $string;
            }
        }

        protected function _triviallyContains(SingleType $type):bool {
            foreach ($this->split() as $t) {
                if ($t->_triviallyContains($type)) {
                    return true;
                }
            }
            return false;
        }

        public function split():array {
            $types = [];
            foreach ($this->types as $type) {
                foreach ($type->split() as $t) {
                    $types[] = $t;
                }
            }
            return $types;
        }
    }

    class Array_ extends SingleType {
        /** @var Type */
        private $inner;

        public function __construct(CodeLoc $loc, Type $inner) {
            parent::__construct($loc);
            $this->inner = $inner;
        }

        public function toTypeHint() {
            if ($this->inner->isMixed()) {
                return 'array';
            } else {
                return null;
            }
        }

        public function toString(bool $atomic = false):string {
            if ($this->inner->isMixed()) {
                return 'array';
            } else {
                return $this->inner->toString(true) . '[]';
            }
        }

        protected function _triviallyContains(SingleType $type):bool {
            return $type instanceof self && $this->inner->triviallyContains($type->inner);
        }
    }
}
