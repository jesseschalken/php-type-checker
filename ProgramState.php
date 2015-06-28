<?php

namespace PhpTypeChecker;

class ProgramState {
    /** @var Variable[] */
    private $vars = [];
    /** @var Variable */
    private $return;
    /** @var Variable */
    private $default;

    function __construct($empty) {
        $this->default = new Variable($empty);
        $this->return  = clone $this->default;
    }

    function __clone() {
        $this->vars    = clone_any($this->vars);
        $this->return  = clone $this->return;
        $this->default = clone $this->default;
    }

    /**
     * @param string $name
     * @return Variable
     */
    function variable($name) {
        if (!array_key_exists($name, $this->vars))
            $this->vars[$name] = clone $this->default;
        return $this->vars[$name];
    }

    /**
     * @param Type[] $types
     */
    function setReturn(array $types) {
        $this->return->set($types);
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
        $this->default->import($that->default);

        $vars = array_keys(array_replace($this->vars, $that->vars));

        foreach ($vars as $name) {
            $this->variable($name)->import($that->variable($name));
        }
    }
}

class ProgramStates {
    /** @var ProgramState */
    private $next;
    /** @var ProgramState[] */
    private $throw = [];
    /** @var ProgramState */
    private $return;
    /** @var ProgramState[] */
    private $break = [];
    /** @var ProgramState[] */
    private $continue = [];

    function __construct() {
        $this->next   = new ProgramState(false);
        $this->return = new ProgramState(true);
    }

    function throw_($exception) {
        if (!array_key_exists($exception, $this->throw))
            $this->throw[$exception] = new ProgramState(true);
        return $this->throw[$exception];
    }

    function break_($level) {
        if (!array_key_exists($level, $this->break))
            $this->break[$level] = new ProgramState(true);
        return $this->break[$level];
    }

    function continue_($level) {
        if (!array_key_exists($level, $this->continue))
            $this->continue[$level] = new ProgramState(true);
        return $this->continue[$level];
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
            throw new \Exception('unhandled expr: ' . typeof($expr));
        }
    }

    /**
     * @param \PhpParser\Node\Expr           $cond
     * @param \PhpParser\Node[]              $ifStmts
     * @param \PhpParser\Node\Stmt\ElseIf_[] $elseIfs
     * @param \PhpParser\Node[]              $elseStmts
     * @throws \Exception
     */
    function executeIf($cond, array $ifStmts, array $elseIfs, array $elseStmts) {
        $else = clone $this;

        $this->evaluate($cond, true);
        $else->evaluate($cond, false);

        $this->executeAll($ifStmts);

        if ($elseIfs) {
            /** @var \PhpParser\Node\Stmt\ElseIf_ $elseIf */
            $elseIf = array_shift($elseIfs);
            $else->executeIf(
                $elseIf->cond,
                $elseIf->stmts,
                $elseIfs,
                $elseStmts
            );
        } else {
            $else->executeAll($elseStmts);
        }

        $this->import($else);
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
        } else if ($stmt instanceof \PhpParser\Node\Stmt\Return_) {
            $expr = $stmt->expr;
            if ($expr)
                $this->next->setReturn($this->evaluate($expr, null));
            $this->return->import($this->next);
            $this->next = new ProgramState(true);
        } else {
            throw new \Exception('unhandled stmt: ' . typeof($stmt));
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
        $state = clone $this->next;
        $state->import(clone $this->return);
        return $state->unparse();
    }

    function __clone() {
        $this->return   = clone $this->return;
        $this->next     = clone $this->next;
        $this->throw    = clone_any($this->throw);
        $this->break    = clone_any($this->break);
        $this->continue = clone_any($this->continue);
    }

    function import(self $that) {
        $this->next->import($that->next);
        $this->return->import($that->return);

        $throws    = array_keys(array_replace($this->throw, $that->throw));
        $breaks    = array_keys(array_replace($this->break, $that->break));
        $continues = array_keys(array_replace($this->continue, $that->continue));

        foreach ($throws as $exception) {
            $this->throw_($exception)->import($that->throw_($exception));
        }

        foreach ($breaks as $level) {
            $this->break_($level)->import($that->break_($level));
        }

        foreach ($continues as $level) {
            $this->continue_($level)->import($that->break_($level));
        }
    }
}
