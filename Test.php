<?php

namespace PhpTypeChecker;

use PhpParser;

class Test extends \PHPUnit_Framework_TestCase {
    private static function assertState($code, array $result) {
        $state  = new ProgramState;
        $parser = new PhpParser\Parser(new PhpParser\Lexer);
        $nodes  = $parser->parse($code);

        foreach ($nodes as $node)
            $state->execute($node);

        self::assertEquals($result, $state->toArray());
    }

    function testVariable() {
        self::assertState(<<<'s'
<?php

$a = 9;

s
            , [
                'locals' => [
                    'types'   => [
                        'a' => [
                            'type'  => 'int',
                            'value' => 9,
                        ],
                    ],
                    'default' => [],
                ],
                'stop'   => null,
            ]);
    }
}
