<?php

namespace JesseSchalken\PhpTypeChecker;

use JesseSchalken\PhpTypeChecker\Definitions\GlobalDefinitions;

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
 * @return string[]
 */
function type_check(array $phpFiles):array {
    $errors = new class () extends ErrorReceiver {
        public $errors = [];

        public function add(string $message, CodeLoc $loc) {
            $this->errors[] = $loc->format($message);
        }
    };
    $files  = File::parse($phpFiles, $errors);
    $defns  = new GlobalDefinitions();
    foreach ($files as $file) {
        $file->gatherDefinitions($defns);
    }
    foreach ($files as $file) {
        $file->typeCheck($defns, $errors);
    }
    return $errors->errors;
}

function extract_namespace(string $name):string {
    $pos = strrpos($name, '\\');
    return $pos === false ? '' : substr($name, 0, $pos);
}

function remove_namespace(string $name):string {
    $pos = strrpos($name, '\\');
    return $pos === false ? $name : substr($name, $pos + 1);
}


