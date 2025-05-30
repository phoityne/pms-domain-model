
module PMS.Domain.Model.DM.TH where


-- |
--
rdrop :: String -> String -> String
rdrop str = reverse . drop (length str) . reverse

-- |
--
dropDataName :: String -> String -> String
dropDataName str = tail . reverse . drop (length str) . reverse

-- |
--
fieldModifier :: String -> String -> String
fieldModifier str  = tail . rdrop str

