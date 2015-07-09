<?php

require_once __DIR__ . '/vendor/autoload.php';

print \FailWhale\Value::introspect(
    \PhpTypeChecker\parse_php(<<<'s'
<?php

/**
 * @param foo
 */
/**
 * @return null
 */
$f = function ()
{
};
s
    )
)->toString();

