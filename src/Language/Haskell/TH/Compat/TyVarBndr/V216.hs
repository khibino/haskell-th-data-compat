-- The latest version of applying this module impl is template-haskell-2.16
module Language.Haskell.TH.Compat.TyVarBndr.V216 (
  unTyVarBndr,
  ) where

import Language.Haskell.TH (Name, TyVarBndr (..))

unTyVarBndr :: TyVarBndr -> (Name, Maybe a)
unTyVarBndr (PlainTV n) = (n, Nothing)
unTyVarBndr (KindedTV n _k) = (n, Nothing)
