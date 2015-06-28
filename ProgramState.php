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
        } else {
            throw new \Exception('unhandled: ' . get_class($expr));
        }
    }

    /**
     * @param string $phpCode
     * @return void
     */
    function process($phpCode) {
        $nodes = parse($phpCode);

        foreach ($nodes as $node) {
            if ($node instanceof \PhpParser\Node\Expr) {
                $this->evaluate($node, null);
            } else {

            }
        }
    }

    function unparse() {
        if ($this->next)
            return $this->next->unparse();
        else
            return 'nope';
    }
}
