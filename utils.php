<?php

namespace PhpTypeChecker;

/**
 * @param mixed $a
 * @return mixed
 */
function clone_any($a) {
    if (is_array($a)) {
        foreach ($a as $k => $v)
            $a[$k] = clone_any($v);
        return $a;
    } else if (is_object($a)) {
        return clone $a;
    } else {
        return $a;
    }
}

/**
 * @param string $phpCode
 * @return \PhpParser\Node[]
 */
function parse_php($phpCode) {
    $parser = new \PhpParser\Parser(new \PhpParser\Lexer);
    return $parser->parse($phpCode);
}

/**
 * @param mixed $x
 * @return string
 * @throws \Exception
 */
function typeof($x) {
    if (is_int($x)) {
        return 'int';
    } else if (is_float($x)) {
        return 'float';
    } else if (is_array($x)) {
        $types = [];
        foreach ($x as $v)
            $types[] = typeof($v);
        $types = array_unique($types, SORT_STRING);
        sort($types, SORT_STRING);
        if (count($types) == 1) {
            return "$types[0][]";
        } else {
            return "(" . join('|', $types) . ")[]";
        }
    } else if (is_resource($x)) {
        return 'resource';
    } else if (is_object($x)) {
        return get_class($x);
    } else if (is_bool($x)) {
        return $x ? 'true' : 'false';
    } else if (is_null($x)) {
        return 'null';
    } else if (is_string($x)) {
        return 'string';
    } else {
        throw new \Exception('i dont understand ' . gettype($x));
    }
}

