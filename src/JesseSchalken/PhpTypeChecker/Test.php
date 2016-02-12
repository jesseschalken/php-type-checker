<?php

namespace JesseSchalken\PhpTypeChecker\Test;

use function JesseSchalken\PhpTypeChecker\type_check;

class Test extends \PHPUnit_Framework_TestCase {
    protected function setUp() {
        ini_set('display_errors', 0);
        parent::setUp();
    }

    function test1() {
        self::assertEquals(type_check(['file1.php' => <<<'s'
<?php

function foo(string $f) {}
foo(8);
s
            ,
        ]), <<<'s'
file1.php(4,5): 8 is incompatible with string
s
        );
    }
}