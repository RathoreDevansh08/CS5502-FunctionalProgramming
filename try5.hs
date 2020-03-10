-- Functor
class Functor f where 
    fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

instance Functor [] where
    fmap = map

-- for record type
instance Functor GovDirectory where
  fmap f oldDirectory = GovDirectory {
    mayor = f (mayor oldDirectory),
    interimMayor = f <$> interimMayor oldDirectory,
    cabinet = f <$> cabinet oldDirectory,
    councilMembers = f <$> councilMembers oldDirectory
  }
