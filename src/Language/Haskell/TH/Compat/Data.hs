{-# LANGUAGE CPP #-}

-- |
-- Module      : Language.Haskell.TH.Compat.Data
-- Copyright   : 2016 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module provides compatibility definitions of
-- data-type declaration templates for before temaplate-haskell-2.11
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
