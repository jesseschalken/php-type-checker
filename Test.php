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
            , $state->unparse() );
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
$a = float|int|string
return string|void
s
            , $state->unparse() );
    }
}
