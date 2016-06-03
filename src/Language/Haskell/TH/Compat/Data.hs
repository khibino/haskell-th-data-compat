{-# LANGUAGE CPP #-}

module Language.Haskell.TH.Compat.Data (
#if MIN_VERSION_template_haskell(2,11,0)
  module Language.Haskell.TH.Compat.Data.Current
#else
  module Language.Haskell.TH.Compat.Data.V210
#endif
  ) where

#if MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH.Compat.Data.Current
#else
import Language.Haskell.TH.Compat.Data.V210
#endif
