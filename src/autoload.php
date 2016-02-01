<?php

spl_autoload_register(function ($cls_) {
    $cls = explode('\\', $cls_);
    $sep = DIRECTORY_SEPARATOR;
    $php = __DIR__ . $sep . join($sep, array_slice($cls, 0, -1)) . '.php';
    if (file_exists($php)) {
        /** @noinspection PhpIncludeInspection */
        require_once $php;
    }
});

require_once __DIR__ . '/JesseSchalken/PhpTypeChecker/functions.php';
