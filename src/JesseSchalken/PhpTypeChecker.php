<?php

namespace JesseSchalken\PhpTypeChecker;

use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Parser;
use JesseSchalken\PhpTypeChecker\Stmt;
use JesseSchalken\PhpTypeChecker\Type;
use JesseSchalken\PhpTypeChecker\Context;

interface HasCodeLoc {
    public function loc():CodeLoc;
}

/**
 * A location in a source file, for error reporting. Statements, expressions, types etc all wrap a CodeLoc by extending
 * from Node. It may be changed from a position in a file to a range, if error reporting so requires.
 */
class CodeLoc implements HasCodeLoc {
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
        return "$this->path($this->line,$this->column): $message";
    }

    public function toDocBlockLocation() {
        return new \phpDocumentor\Reflection\DocBlock\Location($this->line, $this->column);
    }

    public function loc():CodeLoc {
        return $this;
    }
}

/**
 * Base class for all things which can be tracked to some position in a source file.
 */
abstract class Node implements HasCodeLoc {
    /** @var HasCodeLoc */
    private $loc;

    public function __construct(HasCodeLoc $loc) {
        $this->loc = $loc->loc();
    }

    public final function loc():CodeLoc {
        return $this->loc;
    }
}

/**
 * Something to throw errors at in the process of parsing and type checking.
 */
abstract class ErrorReceiver {
    /**
     * @param string     $message
     * @param HasCodeLoc $loc
     */
    public abstract function add(string $message, HasCodeLoc $loc);

    /**
     * Returns an error receiver which reports errors against the given location instead of the one given when
     * add() is called.
     * @param HasCodeLoc $loc
     * @return self
     */
    public final function bind(HasCodeLoc $loc):self {
        return new class($this, $loc) extends ErrorReceiver {
            /** @var ErrorReceiver */
            private $self;
            /** @var HasCodeLoc */
            private $loc;

            public function __construct(ErrorReceiver $self, HasCodeLoc $loc) {
                $this->self = $self;
                $this->loc  = $loc;
            }

            public function add(string $message, HasCodeLoc $loc) {
                $this->self->add($message, $this->loc);
            }
        };
    }
}

class NullErrorReceiver extends ErrorReceiver {
    public function add(string $message, HasCodeLoc $loc) {
    }
}

class File extends Node {
    /**
     * @param string[]      $files
     * @param ErrorReceiver $errors
     * @return File[]
     */
    public static function parse(array $files, ErrorReceiver $errors):array {
        /**
         * @var Parser\ParsedFile[] $parsed
         * @var self[]              $result
         */
        $parsed  = [];
        $defined = new Parser\GlobalDefinedNames;
        $result  = [];
        $context = new Context\Context($errors);
        foreach ($files as $path => $contents) {
            $file = new Parser\ParsedFile($path, $contents, $errors);
            $defined->addNodes($file->nodes);
            $parsed[] = $file;
        }
        foreach ($parsed as $file) {
            $self           = new self($file->nullLoc());
            $self->path     = $file->path;
            $self->shebang  = $file->shebang;
            $self->contents = (new Parser\Parser($file, $defined, clone $context))->parseStmts($self, $file->nodes);
            $result[]       = $self;
        }
        return $result;
    }

    /** @var string */
    private $path;
    /** @var string */
    private $shebang = '';
    /** @var Stmt\Block */
    private $contents;

    public function path():string {
        return $this->path;
    }

    public function unparse():string {
        /** @var \PhpParser\PrettyPrinter\Standard $prettyPrinter */
        $prettyPrinter = new class() extends \PhpParser\PrettyPrinter\Standard {
            public function pStmt_Interface(\PhpParser\Node\Stmt\Interface_ $node) {
                return 'interface ' . $node->name
                . (!empty($node->extends) ? ' extends ' . $this->pCommaSeparated($node->extends) : '')
                . ' {' . $this->pStmts($node->stmts) . "\n" . '}';
            }

            public function pStmt_Function(\PhpParser\Node\Stmt\Function_ $node) {
                return 'function ' . ($node->byRef ? '&' : '') . $node->name
                . '(' . $this->pCommaSeparated($node->params) . ')'
                . (null !== $node->returnType ? ' : ' . $this->pType($node->returnType) : '')
                . ' {' . $this->pStmts($node->stmts) . "\n" . '}';
            }

            public function pStmt_Trait(\PhpParser\Node\Stmt\Trait_ $node) {
                return 'trait ' . $node->name
                . ' {' . $this->pStmts($node->stmts) . "\n" . '}';
            }

            protected function pClassCommon(\PhpParser\Node\Stmt\Class_ $node, $afterClassToken) {
                return $this->pModifiers($node->type)
                . 'class' . $afterClassToken
                . (null !== $node->extends ? ' extends ' . $this->p($node->extends) : '')
                . (!empty($node->implements) ? ' implements ' . $this->pCommaSeparated($node->implements) : '')
                . ' {' . $this->pStmts($node->stmts) . "\n" . '}';
            }

            public function pStmt_ClassMethod(\PhpParser\Node\Stmt\ClassMethod $node) {
                return $this->pModifiers($node->type)
                . 'function ' . ($node->byRef ? '&' : '') . $node->name
                . '(' . $this->pCommaSeparated($node->params) . ')'
                . (null !== $node->returnType ? ' : ' . $this->pType($node->returnType) : '')
                . (null !== $node->stmts
                    ? ' {' . $this->pStmts($node->stmts) . "\n" . '}'
                    : ';');
            }

            public function pExpr_Array(\PhpParser\Node\Expr\Array_ $node) {
                $items = $node->items ? "\n" . $this->pImplode($node->items, ",\n") . ',' : '';
                $items = preg_replace('~\n(?!$|' . $this->noIndentToken . ')~', "\n    ", $items);

                if ($this->options['shortArraySyntax']) {
                    return $items ? "[$items\n]" : "[]";
                } else {
                    return $items ? "array($items\n)" : "array()";
                }
            }
        };
        $parserNodes   = $this->contents->unparseWithNamespaces();
        return $this->shebang . $prettyPrinter->prettyPrintFile($parserNodes);
    }

    /**
     * @param Context\Context $context
     * @return void
     */
    public function gatherGlobalDecls(Context\Context $context) {
        $this->contents->gatherGlobalDecls($context);
    }

    /**
     * @param Context\Context $context
     */
    public function typeCheck(Context\Context $context) {
        $context = $context->withoutLocals($this);
        $this->contents->gatherLocalDecls($context);
        $this->contents->gatherInferedLocals($context);
        $this->contents->checkStmt($context);
    }
}

