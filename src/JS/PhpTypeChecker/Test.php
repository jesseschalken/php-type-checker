<?php

namespace JS\PhpTypeChecker;

use JS\PhpTypeChecker\Node\Parser;

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