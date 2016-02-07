<?php
/**
 * Created by PhpStorm.
 * User: Jesse
 * Date: 7/02/2016
 * Time: 7:50 PM
 */
namespace JesseSchalken\PhpTypeChecker\Defns;

abstract class Definition extends \JesseSchalken\PhpTypeChecker\Stmt\SingleStmt {
    public abstract function name():\JesseSchalken\PhpTypeChecker\Stmt\string;

    public final function namespace_():\JesseSchalken\PhpTypeChecker\Stmt\string {
        return extract_namespace($this->name());
    }

    public final function findDeclarations():array {
        $decls   = parent::findDeclarations();
        $decls[] = $this;
        return $decls;
    }
}