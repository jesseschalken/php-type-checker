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
else if (7)
    $a = 8.6;
else
    $a = 7;
s
        );
        self::assertEquals(<<<'s'
$a = string|float|int
return void
s
            , $state->unparse() );
    }
}
