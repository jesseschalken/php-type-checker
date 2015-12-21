<?php

namespace JesseSchalken\PhpTypeChecker;

use JesseSchalken\PhpTypeChecker\Node\Parser;

class Test extends \PHPUnit_Framework_TestCase {
    function testParse() {
        Parser::parse(<<<'s'
<?php

if (true) {
    $a = 8;
} else {
    return;
}
s
        );
    }
}