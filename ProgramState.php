<?php

namespace PhpTypeChecker;

class ProgramState {
    /** @var Variable[] */
    private $vars = [];
    /** @var Variable */
    private $return;

    function __construct() {
        $this->return = new Variable;
    }

    function __clone() {
        $this->vars   = clone_array($this->vars);
        $this->return = clone $this->return;
    }

    /**
     * @param string $name
     * @return Variable
     */
    function variable($name) {
        if (!array_key_exists($name, $this->vars))
            $this->vars[$name] = new Variable;
        return $this->vars[$name];
    }

    function unparse() {
        ksort($this->vars, SORT_STRING);
        $lines = [];
        foreach ($this->vars as $name => $val)
            $lines[] = "\$$name = " . $val->unparse();
        $lines[] = "return " . $this->return->unparse();
        return join("\n", $lines);
    }

    function import(self $that) {
        $this->return->import($that->return);

        foreach ($that->vars as $name => $var) {
            $this->variable($name)->import($var);
        }
    }
}

class ProgramStates {
    /** @var ProgramState|null */
    private $next;
    /** @var ProgramState[] */
    private $throw = [];
    /** @var ProgramState|null */
    private $return;
    /** @var ProgramState[] */
    private $break = [];
    /** @var ProgramState[] */
    private $continue = [];

    function __construct() {
        $this->next = new ProgramState;
    }

    /**
     * @param \PhpParser\Node\Expr $expr
     * @param bool|null            $assume
     * @return Type[]
     * @throws \Exception
     */
    function evaluate(\PhpParser\Node\Expr $expr, $assume) {
        if ($expr instanceof \PhpParser\Node\Expr\Assign) {
            $var = $expr->var;
            $var = $var instanceof \PhpParser\Node\Expr\Variable ? $var->name : '__unknown';
            $var = $this->next->variable($var);
            $val = $this->evaluate($expr->expr, $assume);
            $var->set($val);
            return $val;
        } else if ($expr instanceof \PhpParser\Node\Scalar\String_) {
            return [new String];
        } else if ($expr instanceof \PhpParser\Node\Scalar\LNumber) {
            return [new Int];
        } else if ($expr instanceof \PhpParser\Node\Scalar\DNumber) {
            return [new Float];
        } else {
            throw new \Exception('unhandled expr: ' . get_class($expr));
        }
    }

    /**
     * @param \PhpParser\Node\Expr           $cond
     * @param \PhpParser\Node[]              $stmts
     * @param \PhpParser\Node\Stmt\ElseIf_[] $elseIfs
     * @param \PhpParser\Node[]              $else
     * @throws \Exception
     */
    function executeIf($cond, array $stmts, array $elseIfs, array $else) {
        $false = clone $this;

        $this->evaluate($cond, true);
        $false->evaluate($cond, false);

        $this->executeAll($stmts);

        if ($elseIfs) {
            /** @var \PhpParser\Node\Stmt\ElseIf_ $elseIf */
            $elseIf = array_shift($elseIfs);
            $false->executeIf(
                $elseIf->cond,
                $elseIf->stmts,
                $elseIfs,
                $else
            );
        } else {
            $false->executeAll($else);
        }

        $this->import($false);
    }

    /**
     * @param \PhpParser\Node\Expr|\PhpParser\Node\Stmt $stmt
     * @throws \Exception
     */
    function execute($stmt) {
        if ($stmt instanceof \PhpParser\Node\Expr) {
            $this->evaluate($stmt, null);
        } else if ($stmt instanceof \PhpParser\Node\Stmt\If_) {
            $this->executeIf(
                $stmt->cond,
                $stmt->stmts,
                $stmt->elseifs,
                $stmt->else ? $stmt->else->stmts : []
            );
        } else {
            throw new \Exception('unhandled stmt: ' . get_class($stmt));
        }
    }

    function executeAll(array $stmts) {
        foreach ($stmts as $stmt)
            $this->execute($stmt);
    }

    /**
     * @param string $phpCode
     * @return void
     */
    function process($phpCode) {
        $this->executeAll(parse_php($phpCode));
    }

    function unparse() {
        if ($this->next)
            return $this->next->unparse();
        else
            return 'nope';
    }

    function __clone() {
        if ($this->return)
            $this->return = clone $this->return;
        if ($this->next)
            $this->next = clone $this->next;
        $this->throw    = clone_array($this->throw);
        $this->break    = clone_array($this->break);
        $this->continue = clone_array($this->continue);
    }

    function import(self $that) {
        if ($that->next) {
            if ($this->next)
                $this->next->import($that->next);
            else
                $this->next = $that->next;
        }

        if ($that->return) {
            if ($this->return)
                $this->return->import($that->return);
            else
                $this->return = $that->return;
        }

        foreach ($that->throw as $exception => $state) {
            if (array_key_exists($exception, $this->throw)) {
                $this->throw[$exception]->import($state);
            } else {
                $this->throw[$exception] = $state;
            }
        }

        foreach ($that->break as $level => $state) {
            if (array_key_exists($level, $this->break)) {
                $this->break[$level]->import($state);
            } else {
                $this->break[$level] = $state;
            }
        }

        foreach ($that->continue as $level => $state) {
            if (array_key_exists($level, $this->continue)) {
                $this->continue[$level]->import($state);
            } else {
                $this->continue[$level] = $state;
            }
        }
    }
}
