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
}
