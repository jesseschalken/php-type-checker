<?php

namespace PhpTypeChecker;

/**
 * @param array $a
 * @return array
 */
function clone_array(array $a) {
    foreach ($a as $k => $v) {
        if (is_array($v))
            $a[$k] = clone_array($v);
        else if (is_object($v))
            $a[$k] = clone $v;
    }
    return $a;
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
        switch (count($types)) {
            case 0:
                return 'void[]';
            case 1:
                return "$types[0][]";
            default:
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

/**
 * @param callable $callback
 * @param array $array
 * @return array
 */
function array_map_merge(callable $callback, array $array) {
    return array_merge_many(array_map($callback, $array));
}

/**
 * @param array[] $arrays
 * @return array
 */
function array_merge_many(array $arrays) {
    return call_user_func_array('array_merge', array_values($arrays));
}


