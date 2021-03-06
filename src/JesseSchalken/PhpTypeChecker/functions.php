<?php

namespace JesseSchalken\PhpTypeChecker;

use JesseSchalken\PhpTypeChecker\Context;

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

function str_eq(string $a, string $b):bool {
    return strcmp($a, $b) == 0;
}

function str_ieq(string $a, string $b):bool {
    return strcasecmp($a, $b) == 0;
}

function normalize_constant(string $name):string {
    // $name is the name of the constant including the namespace.
    // Namespaces are case insensitive, but constants are case sensitive,
    // therefore split the name after the last "\" and strtolower() the left side.
    $pos = strrpos($name, '\\');
    $pos = $pos === false ? 0 : $pos + 1;

    $prefix   = substr($name, 0, $pos);
    $constant = substr($name, $pos);

    return strtolower($prefix) . $constant;
}

/**
 * @param string[] $phpFiles
 * @return string
 */
function type_check(array $phpFiles):string {
    $errors  = new class () extends ErrorReceiver {
        public $errors = [];

        public function add(string $message, HasCodeLoc $loc) {
            $this->errors[] = $loc->loc()->format($message);
        }
    };
    $files   = File::parse($phpFiles, $errors);
    $context = new Context\Context($errors);
    foreach ($files as $file) {
        $file->gatherGlobalDecls($context);
    }
    foreach ($files as $file) {
        $file->typeCheck($context);
    }
    return join("\n", $errors->errors);
}

function extract_namespace(string $name):string {
    $pos = strrpos($name, '\\');
    return $pos === false ? '' : substr($name, 0, $pos);
}

function remove_namespace(string $name):string {
    $pos = strrpos($name, '\\');
    return $pos === false ? $name : substr($name, $pos + 1);
}

/**
 * @param Type\Type[]     $types1
 * @param Type\Type[]     $types2
 * @param Context\Context $context
 * @return Type\Type[]
 */
function merge_types(array $types1, array $types2, Context\Context $context):array {
    foreach ($types2 as $key => $type) {
        $types1[$key] = isset($types1[$key])
            ? $types1[$key]->addType($type, $context)
            : $type;
    }
    return $types1;
}

function to_array_key($value):string {
    return (string)(array_keys([$value => null])[0]);
}

