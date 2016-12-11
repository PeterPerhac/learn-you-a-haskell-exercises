--
-- new YesNo typeclass
-- 

class YesNo a where
    yesno :: a -> Bool


-- 
-- and some types of the YesNo class
--

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False  


--
-- additional functions
--

yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult  

-- the YesNo typeclass is not derivable so the below will not work:
-- data Color = Red | Green | Blue deriving (Enum, Ord, Eq, Bounded, YesNo)

