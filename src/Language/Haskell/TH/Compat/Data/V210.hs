module Language.Haskell.TH.Compat.Data.V210 (
  dataD', newtypeD', dataInstD', newtypeInstD',
  ) where

import Language.Haskell.TH
  (dataD, newtypeD, dataInstD, newtypeInstD,
   CxtQ, Name, TyVarBndr, ConQ, TypeQ, DecQ)


-- | Compatible definition of 'dataD' for before temaplate-haskell-2.11
dataD' :: CxtQ -> Name -> [TyVarBndr] -> [ConQ] -> [Name]
       -> DecQ
dataD' = dataD

-- | Compatible definition of 'newtypeD' for before temaplate-haskell-2.11
newtypeD' :: CxtQ -> Name -> [TyVarBndr] -> ConQ -> [Name]
          -> DecQ
newtypeD' = newtypeD

-- | Compatible definition of 'dataInstD' for before temaplate-haskell-2.11
dataInstD' :: CxtQ -> Name -> [TypeQ] -> [ConQ] -> [Name]
           -> DecQ
dataInstD' = dataInstD

-- | Compatible definition of 'newtypeInstD' for before temaplate-haskell-2.11
newtypeInstD' :: CxtQ -> Name -> [TypeQ] -> ConQ -> [Name]
              -> DecQ
newtypeInstD' = newtypeInstD
