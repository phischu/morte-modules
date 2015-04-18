{-# LANGUAGE OverloadedStrings #-}
module MorteModules where

import Morte.Core
import Morte.Parser
import Data.Text.Lazy


data Module =
    ModuleVar Var |
    Binding Binding |
    Project Module Var |
    ModuleLam Var Signature Module |
    ModuleApp Module Module |
    Sealing Var Signature

data Binding =
    Value Var Expr |
    Type Var Expr |
    Module Var Module |
    Signature Var Signature |
    Empty |
    Sequence Binding Binding |
    Include Module

data Signature =
    WhereType Signature Var Expr

data ExprOrPath = Expr Expr | Path [Var]

data Record a = Record [(Var,a)]




arrow :: Expr
arrow = Lam "a" (Const Star) (Lam "b" (Const Star) (Pi "_" (Var "a") (Var "b")))

natural :: Expr
natural = Pi "a" (Const Star) (arrow `App` (arrow `App` (Var "a") `App` (Var "a")) `App` (arrow `App` (Var "a") `App` (Var "a")))

unsafeParse :: Text -> Expr
unsafeParse = either (error . show) id . exprFromText

n :: Expr
n = unsafeParse "forall (a : *) -> (a -> a) -> a -> a"


