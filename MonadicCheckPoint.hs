{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
undefined = undefined

class Functor f where
    fmap :: (a -> b) -> f a -> f b

--Laws:
--    forall f :: (a -> b), pac1 :: (a -> f a), pac2 :: (b -> f b) .
--        fmap f . pac1 == pac2 . f
-- Law of the commuting square:
--       f
-- a  -------> b
-- |           |
-- | p         | p
-- | a         | a
-- | c         | c
-- | 1         | 2
-- V   fmap f  V
-- F a -----> F b

{-
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

--Laws:
--    fmap == (<*>) . pure

--------------------------------------
-- or else:

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

--{- -- This is a nice pattern to comment out/uncomment something.
instance Applicative f => Functor f where -- This means
    fmap = (<*>) . pure                   -- I can deduce `Functor f` if
                                          -- given `Applicative f`.
---}
--------------------------------------

class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
    id = \x -> x
    (.) f g x = f (g x)

--Laws:
-- 1) (.) f g x = f (g x) :  ]g = id    =>  f . id = f
-- 2) (.) f g x = f (g x) :  ]f = id    =>  id . g = g
-- 3) (.) f g x = f (g x) :  ]f = a . b =>  (a . b) . g = a . (b . g)

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
a >>> b = b . a

class Monad1 m where
    return1 :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad1 m => m a -> m b -> m b
a >> b = a >>= (\_ -> b)

class Functor m => Monad2 m where -- This means `fmap` is avaible
    return2 :: a -> m a            -- in `Monad2` too.
    join :: m (m a) -> m a

class Monad3 m where
    return3 :: a -> m a
    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
--                f            g           f >=> g
--Laws:
--   1) (>=>): ]a = b        => return3 >=> g = g
--   2) (>=>): ]b = c        => f >=> return3 = f
--   3) (>=>): ]f = u >=> v  => (u >=> v) >=> g = u >=> (v >=> g) 
--       comment:  [(a -> m b') -> (b' -> m b)] -> (b -> m c) = (a -> m b') -> [(b' -> m b) -> (b -> m c)] 
--                                    [a -> m b] -> (b -> mc) = (a -> m b') -> [b' -> mc]
--
-- Show the relation between Monad3 and Category:
-- 1) cat a b = a -> m b
-- 2) id = cat a a = a -> ma = return3 a ===> id = return3
-- 3) *(.) :: cat b c -> cat a b -> cat a c             
--    *(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
--    ====>    f >=> g = g . f    where f = cat a b = a -> m b, g = cat b c = b -> m c 

-- Instances from Monad1 to anything are allowed to use do-syntax in
-- right-hand side of the equations.
	
instance Monad1 m => Monad2 m where
    return2 = return1
    join mma = mma >>= id


instance Monad1 m => Functor m where
-- fmap f ma = mb
-- (>>=) :: m a -> (a -> m b) -> m b
-- need: m a -> m b 
-- m a -> (a -> m(f a)) -> m(f a)
-- a -> m(f a) = return1 . f : a --> f a --> m(f a) 
    fmap f ma = ma >>= (return1 . f)


instance Monad1 m => Applicative m where
    pure = return1
-- <*> :: m (a -> b) -> m a -> m b
-- f <*> ma = mb
-- need: ma >>= (a -> m b) = mb
-- given f = m (a -> b) = m t can convert to m b : f >>= g = m b
-- g :: t -> m b = \t -> (return1 (t a)) where t = a -> b
    f <*> ma = ma >>= (\a -> (f >>= (\t -> return1 (t a)))) 


instance Monad2 m => Monad1 m where
    return1 = return2
-- (>>=) :: m a' -> (a' -> m b) -> m b
-- a = m a', f = a' -> m b
-- fmap f (m a') : (a' -> m b) -> m a' -> m (m b)
-- join : m (m b) -> m b
    a >>= f = join (fmap f a) 


instance Monad3 m => Monad1 m where
    return1 = return3
-- (>>=) :: m a -> (a -> m b) -> m b
-- need g :: m a -> m b
-- (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
-- ]a = ma, b = a, c = b : (m a -> m a) -> (a -> m b) -> (m a -> m b)    
--                            id               f              g  
    ma >>= f = (id >=> f) ma  


instance Monad1 m => Monad3 m where
    return3 = return1
-- (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
-- (>>=) :: m a -> (a -> m b) -> m b
-- need t :: a -> m c
-- m c =  m b -> (b -> m c) = mb >>= g 
-- m b = m a -> (a -> m b) = ma >>= f
-- m a = return1 a   
    f >=> g = \a -> ((return1 a) >>= f) >>= g
