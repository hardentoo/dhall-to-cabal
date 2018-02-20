{-# language DeriveFunctor #-}
{-# language OverloadedStrings #-}

module Test where

import Control.Applicative
import Data.Functor.Identity
import Data.Function ((&))
import Dhall.Core hiding (Const)
import qualified Dhall.Core as Expr
import Dhall.Parser
import Data.Monoid

import Control.Lens hiding (Const(..), rewrite)

data Tree a = Leaf a | Tree { tCond :: a, tTrue :: Tree a, tFalse :: Tree a }
  deriving (Functor, Show)

-- rewrite :: Expr s a -> Expr s a
rewrite e = 
  let
    saturated =
      normalize (App (shift 1 "x" e) (Var "x"))

    loop e =
      let
        condition =
          getConst $ 
          deepOf
            subExpr
            (findApp (Var "x"))
            (Const . First . Just) e
      in case condition of
        First Nothing -> Leaf e
        First (Just cond) -> do
          Tree cond
               (loop (runIdentity (deepOf subExpr (exactly cond) (const (pure (BoolLit True))) e)))
               (loop (runIdentity (deepOf subExpr (exactly cond) (const (pure (BoolLit False))) e)))

  in shift (-1) "x" <$> loop saturated

Right e = fmap normalize (exprFromText mempty "\\ ( foo : Int -> Bool ) -> foo 5 || foo 10")

findApp x f e@(App x' _) | x == x' = f e
findApp _ _ e = pure e

exactly x f e | x == e = f e
exactly _ _ e = pure e

subExpr f c@Expr.Const{} = pure c
subExpr f (BoolAnd a b) = BoolAnd <$> f a <*> f b
subExpr f (BoolOr a b) = BoolOr <$> f a <*> f b
subExpr f (NaturalPlus a b) = NaturalPlus <$> f a <*> f b
subExpr f (App a b) = App <$> f a <*> f b 
subExpr _ e = pure e
-- data Expr s a
--   = Const Const
--   | Var Var
--   | Lam Data.Text.Internal.Lazy.Text (Expr s a) (Expr s a)
--   | Pi Data.Text.Internal.Lazy.Text (Expr s a) (Expr s a)
--   | App (Expr s a) (Expr s a)
--   | Let Data.Text.Internal.Lazy.Text
--         (Maybe (Expr s a))
--         (Expr s a)
--         (Expr s a)
--   | Annot (Expr s a) (Expr s a)
--   | Bool
--   | BoolLit Bool
--   | BoolOr (Expr s a) (Expr s a)
--   | BoolEQ (Expr s a) (Expr s a)
--   | BoolNE (Expr s a) (Expr s a)
--   | BoolIf (Expr s a) (Expr s a) (Expr s a)
--   | Natural
--   | NaturalLit GHC.Natural.Natural
--   | NaturalFold
--   | NaturalBuild
--   | NaturalIsZero
--   | NaturalEven
--   | NaturalOdd
--   | NaturalToInteger
--   | NaturalShow
--   | NaturalTimes (Expr s a) (Expr s a)
--   | Integer
--   | IntegerShow
--   | Double
--   | DoubleLit scientific-0.3.5.2:Data.Scientific.Scientific
--   | DoubleShow
--   | Text
--   | TextLit (Chunks s a)
--   | TextAppend (Expr s a) (Expr s a)
--   | List
--   | ListLit (Maybe (Expr s a)) (Data.Vector.Vector (Expr s a))
--   | ListAppend (Expr s a) (Expr s a)
--   | ListBuild
--   | ListFold
--   | ListLength
--   | ListHead
--   | ListLast
--   | ListIndexed
--   | ListReverse
--   | Optional
--   | OptionalLit (Expr s a) (Data.Vector.Vector (Expr s a))
--   | OptionalFold
--   | OptionalBuild
--   | Record (Data.HashMap.Strict.InsOrd.InsOrdHashMap
--               Data.Text.Internal.Lazy.Text (Expr s a))
--   | RecordLit (Data.HashMap.Strict.InsOrd.InsOrdHashMap
--                  Data.Text.Internal.Lazy.Text (Expr s a))
--   | Union (Data.HashMap.Strict.InsOrd.InsOrdHashMap
--              Data.Text.Internal.Lazy.Text (Expr s a))
--   | UnionLit Data.Text.Internal.Lazy.Text
--              (Expr s a)
--              (Data.HashMap.Strict.InsOrd.InsOrdHashMap
--                 Data.Text.Internal.Lazy.Text (Expr s a))
--   | Combine (Expr s a) (Expr s a)
--   | Prefer (Expr s a) (Expr s a)
--   | Merge (Expr s a) (Expr s a) (Maybe (Expr s a))
--   | Constructors (Expr s a)
--   | Field (Expr s a) Data.Text.Internal.Lazy.Text
--   | Note s (Expr s a)
--   | Embed a
