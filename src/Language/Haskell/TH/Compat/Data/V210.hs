module Language.Haskell.TH.Compat.Data.V210 (
  dataD', newtypeD', dataInstD', newtypeInstD',
  ) where

import Language.Haskell.TH
  (dataD, newtypeD, dataInstD, newtypeInstD,
   CxtQ, Name, TyVarBndr, ConQ, TypeQ, DecQ)


-- | Definition against 'dataD',
--   compatible with before temaplate-haskell-2.11
dataD' :: CxtQ -> Name -> [TyVarBndr] -> [ConQ] -> [Name]
       -> DecQ
dataD' = dataD

-- | Definition against 'newtypeD',
--   compatible with before temaplate-haskell-2.11
newtypeD' :: CxtQ -> Name -> [TyVarBndr] -> ConQ -> [Name]
          -> DecQ
newtypeD' = newtypeD

-- | Definition against 'dataInstD',
--   compatible with before temaplate-haskell-2.11
dataInstD' :: CxtQ -> Name -> [TypeQ] -> [ConQ] -> [Name]
           -> DecQ
dataInstD' = dataInstD

-- | Definition against 'newtypeInstD',
--   compatible with before temaplate-haskell-2.11
newtypeInstD' :: CxtQ -> Name -> [TypeQ] -> ConQ -> [Name]
              -> DecQ
newtypeInstD' = newtypeInstD
