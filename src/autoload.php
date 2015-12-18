<?php

spl_autoload_register(function ($cls) {
    for ($cls = explode('\\', $cls); $cls; array_pop($cls)) {
        $php = __DIR__ . DIRECTORY_SEPARATOR . join(DIRECTORY_SEPARATOR, $cls) . '.php';
        if (file_exists($php)) {
            /** @noinspection PhpIncludeInspection */
            require_once $php;
            break;
        }
    }
});
