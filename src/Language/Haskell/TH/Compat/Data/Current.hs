module Language.Haskell.TH.Compat.Data.Current (
  dataD', newtypeD', dataInstD', newtypeInstD',
  ) where

import Language.Haskell.TH
  (dataD, newtypeD, dataInstD, newtypeInstD, conT,
   CxtQ, Name, TyVarBndr, ConQ, TypeQ, DecQ)


-- | Definition against 'dataD',
--   compatible with before temaplate-haskell-2.11
dataD' :: CxtQ -> Name -> [TyVarBndr] -> [ConQ] -> [Name]
       -> DecQ
dataD' cxt n bs cs ds = dataD cxt n bs Nothing cs $ mapM conT ds

-- | Definition against 'newtypeD',
--   compatible with before temaplate-haskell-2.11
newtypeD' :: CxtQ -> Name -> [TyVarBndr] -> ConQ -> [Name]
          -> DecQ
newtypeD' cxt n bs c ds = newtypeD cxt n bs Nothing c $ mapM conT ds

-- | Definition against 'dataInstD',
--   compatible with before temaplate-haskell-2.11
dataInstD' :: CxtQ -> Name -> [TypeQ] -> [ConQ] -> [Name]
           -> DecQ
dataInstD' cxt n as cs ds = dataInstD cxt n as Nothing cs $ mapM conT ds

-- | Definition against 'newtypeInstD',
--   compatible with before temaplate-haskell-2.11
newtypeInstD' :: CxtQ -> Name -> [TypeQ] -> ConQ -> [Name]
              -> DecQ
newtypeInstD' cxt n as c ds = newtypeInstD cxt n as Nothing c $ mapM conT ds
