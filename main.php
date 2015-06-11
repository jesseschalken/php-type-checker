<?php

require_once __DIR__ . '/vendor/autoload.php';

$code = <<<'s'
<?php

/**
 * @param string $blah
 * @param int $foo
 */
function foo($blah, $foo) {
}

foo(9);

s;

$parser = new PhpParser\Parser(new PhpParser\Lexer);
$parsed = $parser->parse($code);

$defs = new Definitions;
$defs->import($parsed);
$defs->verify($parsed, new ErrorReceiver('test'));

class ErrorReceiver {
    private $file;

    function __construct($file) {
        $this->file = $file;
    }

    function error($line, $message) {
        print "$this->file:$line  $message\n";
    }
}

class Function_ {
    /** @var \PhpParser\Node\Stmt\Function_ */
    private $definition;
    /** @var Type[] */
    private $args = array();

    function __construct(\PhpParser\Node\Stmt\Function_ $definition) {
        $this->definition = $definition;

        $doc = $this->parseDocTypes();

        foreach ($definition->params as $param) {
            if (isset($doc[$param->name])) {
                $type = $doc[$param->name];
            } else {
                $type = new Mixed;
            }

            $this->args[$param->name] = $type;
        }
    }

    function parseDocTypes() {
        $comment = $this->definition->getDocComment();
        if (!$comment)
            return [];
        $phpDoc = new phpDocumentor\Reflection\DocBlock($comment->getText());
        /** @var Type[] $args */
        $args = array();
        foreach ($phpDoc->getTagsByName('param') as $param) {
            if (!$param instanceof \phpDocumentor\Reflection\DocBlock\Tag\ParamTag)
                continue;
            $name = ltrim($param->getVariableName(), '$');
            $args[$name] = Type::parse($param->getType());
        }
        return $args;
    }

    function name() {
        return $this->definition->name;
    }

    /**
     * @param Type[] $args
     * @param ErrorReceiver $errors
     */
    function verify(array $args, $line, ErrorReceiver $errors) {
        foreach ($this->definition->params as $k => $param) {
            if (isset($args[$k])) {
                $needs = $this->args[$param->name];
                $given = $args[$k];

                if (!$needs->contains($given)) {
                    $errors->error($line, "'{$given->toString()}' is incompatible with '{$needs->toString()}'");
                }
            } else if ($param->default === null) {
                $n = $k + 1;
                $errors->error($line, "Missing argument $n (\$$param->name) to function {$this->name()}()");
            }
        }
    }
}

abstract class Type {
    /**
     * @param string $type
     * @return Type
     * @throws Exception
     */
    static function parse($type) {
        if ($type === 'int')
            return new Int;
        else if ($type === 'float')
            return new Float;
        else if ($type === 'mixed')
            return new Mixed;
        else if ($type === 'string')
            return new String_;
        else
            throw new Exception("'$type' is an invalid type");
    }

    /**
     * Whether the other type is contained in this one.
     * @param Type $other
     * @return bool
     */
    function contains(self $other) {
        return false;
    }

    /**
     * @return string
     */
    abstract function toString();
}

class Mixed extends Type {
    function toString() {
        return 'mixed';
    }
}

class String_ extends Type {
    function contains(Type $other) {
        if ($other instanceof self) {
            return true;
        } else {
            return parent::contains($other);
        }
    }

    function toString() { return 'string'; }
}

class Int extends Type {
    function contains(Type $other) {
        if ($other instanceof self) {
            return true;
        } else {
            return parent::contains($other);
        }
    }

    function toString() { return 'int'; }
}

class Float extends Type {
    function contains(Type $other) {
        if ($other instanceof self) {
            return true;
        } else {
            return parent::contains($other);
        }
    }

    function toString() { return 'float'; }
}

class Expr {
    private $expr;

    function __construct(PhpParser\Node\Expr $expr) {
        $this->expr = $expr;
    }

    function inferType() {
        $expr = $this->expr;
        if ($expr instanceof PhpParser\Node\Scalar\String_) {
            return new String_;
        } else if ($expr instanceof PhpParser\Node\Scalar\LNumber) {
            return new Int;
        } else if ($expr instanceof PhpParser\Node\Scalar\DNumber) {
            return new Float;
        } else {
            throw new Exception('I dont know how to infer a type here');
        }
    }
}

class Definitions {
    /** @var Function_[] */
    private $functions = array();

    /**
     * @param PhpParser\Node[] $nodes
     */
    function import(array $nodes) {
        foreach ($nodes as $node) {
            if ($node instanceof PhpParser\Node\Stmt\Function_) {
                $this->functions[$node->name] = new Function_($node);
            }
        }
    }

    /**
     * @param PhpParser\Node[] $nodes
     * @param ErrorReceiver $errors
     */
    function verify(array $nodes, ErrorReceiver $errors) {
        foreach ($nodes as $node) {
            if ($node instanceof PhpParser\Node\Expr\FuncCall) {
                /** @var Type[] $types */
                $types = array();
                foreach ($node->args as $k => $arg) {
                    $types[$k] = (new Expr($arg->value))->inferType();
                }

                $this->functions[join('\\', $node->name->parts)]->verify($types, $node->getLine(), $errors);
            }
        }
    }
}

