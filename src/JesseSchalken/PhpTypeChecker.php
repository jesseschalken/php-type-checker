<?php

namespace JesseSchalken\PhpTypeChecker;

use JesseSchalken\PhpTypeChecker\Expr;
use JesseSchalken\PhpTypeChecker\Stmt;
use JesseSchalken\PhpTypeChecker\Type;
use JesseSchalken\PhpTypeChecker\Parser;

/**
 * A location in a source file, for error reporting. Statements, expressions, types etc all wrap a CodeLoc by extending
 * from Node. It may be changed from a position in a file to a range, if error reporting so requires.
 */
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

/**
 * Base class for all things which can be tracked to some position in a source file.
 */
abstract class Node {
    /** @var CodeLoc */
    private $loc;

    public function __construct(CodeLoc $loc) {
        $this->loc = $loc;
    }

    public final function loc():CodeLoc {
        return $this->loc;
    }
}

/**
 * Something to throw errors at in the process of parsing and type checking.
 */
abstract class ErrorReceiver {
    public abstract function add(string $message, CodeLoc $loc);
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
        foreach ($files as $path => $contents) {
            $file = new Parser\ParsedFile($path, $contents, $errors);
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
     * @param Decls\GlobalDecls $defns
     * @return void
     */
    public function gatherDefinitions(Decls\GlobalDecls $defns) {
    }

    /**
     * @param Decls\GlobalDecls $defns
     * @param ErrorReceiver     $errors
     * @return void
     */
    public function typeCheck(Decls\GlobalDecls $defns, ErrorReceiver $errors) {
    }
}

