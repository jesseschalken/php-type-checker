<?php

namespace JesseSchalken\PhpTypeChecker\Test;

use function JesseSchalken\PhpTypeChecker\type_check;

class Test extends \PHPUnit_Framework_TestCase {
    protected function setUp() {
        ini_set('display_errors', 0);
        parent::setUp();
    }

    public function testTypeHint() {
        self::assertErrors(
            'file1.php',
            <<<'s'
<?php

function foo(string $f) {}
foo(8);
s
            ,
            <<<'s'
file1.php(4,5): 8 is incompatible with string
s
        );
    }

    public function testPhpDoc() {
        self::assertErrors(
            'file1.php',
            <<<'s'
<?php

/**
 * @param int $f
 */
function foo($f) {}
foo('hello');
s
            ,
            <<<'s'
file1.php(7,5): 'hello' is incompatible with int
s
        );
    }

    public function testUndefinedVariable() {
        self::assertErrors(
            'foo.php',
            <<<'s'
<?php
$foo;
s
            ,
            <<<'s'
foo.php(2,1): Undefined variable: foo
s
        );
    }

    public function testVariable() {
        self::assertErrors(
            'test.php',
            <<<'s'
<?php

function foo(string $s) { bar($s); }
function bar(int $s) {}
s
            ,
            <<<'s'
test.php(3,31): string is incompatible with int
s
            );
    }

    private static function assertErrors(string $file, string $contents, string $errors) {
        self::assertEquals($errors, type_check([$file => $contents]));
    }
}