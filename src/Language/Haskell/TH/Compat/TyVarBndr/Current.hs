module Language.Haskell.TH.Compat.TyVarBndr.Current (
  unTyVarBndr,
  ) where

import Language.Haskell.TH (Name, TyVarBndr (..))

unTyVarBndr :: TyVarBndr a -> (Name, Maybe a)
unTyVarBndr (PlainTV n f) = (n, Just f)
unTyVarBndr (KindedTV n f _k) = (n, Just f)
