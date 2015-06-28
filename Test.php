<?php

namespace PhpTypeChecker;

class Test extends \PHPUnit_Framework_TestCase {
    function testAssignment() {
        $state = new ProgramStates;
        $state->process(<<<'s'
<?php
$a = 'b';
s
        );
        self::assertEquals(<<<'s'
$a = string
return void
s
            , $state->unparse());
    }

    function testIf() {
        $state = new ProgramStates;
        $state->process(<<<'s'
<?php
if (5)
    $a = 'b';
elseif (7)
    $a = 8.6;
elseif (2)
    $a = 7;
else
    return 'an hero';
s
        );
        self::assertEquals(<<<'s'
$a = float|int|string|void
return string|void
s
            , $state->unparse());
    }

    function testWhile() {
        $state = new ProgramStates;
        $state->process(<<<'s'
<?php

$t = 1;

$a = 8.2;
$b = 9;
$c = 'hello';

while ($t = 't') {
    $a = $b;
    $b = $c;
}

s
        );
        self::assertEquals(<<<'s'
$a = float|int|string
$b = int|string
$c = string
$t = string
return void
s
            , $state->unparse());
    }
}
