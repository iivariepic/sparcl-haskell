module AddLSB where

{-
Hand-Compiled from:
    Examples/AddLSB.sparcl
-}
main :: IO ()
main = putStrLn ("add43: " ++ show add43 ++ "\nsub73: " ++ show sub73)

data Bit = B0 | B1 deriving Show
data List a = Nil | Cons a (List a) deriving Show

type LSB = List Bit

inc = (inc_fwd, inc_bwd)

inc_fwd :: LSB -> LSB
inc_fwd Nil         = Cons B1 Nil
inc_fwd (Cons B0 x) = Cons B1 x
inc_fwd (Cons B1 x) = Cons B0 (inc_fwd x)

inc_bwd :: LSB -> LSB
inc_bwd r = case r of
    Cons B1 Nil         -> Nil
    Cons B1 (Cons a as) -> Cons B0 (Cons a as)
    Cons B0 y           -> Cons B1 (inc_bwd y)

liftBitList :: LSB -> LSB
liftBitList Nil          = Nil
liftBitList (Cons B0 xs) = Cons B0 (liftBitList xs)
liftBitList (Cons B1 xs) = Cons B1 (liftBitList xs)

eqBitList :: LSB -> LSB -> Bool
eqBitList Nil Nil                   = True
eqBitList (Cons B0 as) (Cons B0 bs) = eqBitList as bs
eqBitList (Cons B1 as) (Cons B1 bs) = eqBitList as bs
eqBitList _ _                       = False

addBitU :: Bit -> LSB -> LSB
addBitU B0 as = as
addBitU B1 as = fwd inc as

-- Assumption: any sublist of as must be Nil if it represents 0
add = (add_fwd, add_bwd)

add_fwd :: Bit -> LSB -> LSB -> LSB
add_fwd B0 as Nil                 = as
add_fwd B1 as Nil                 = inc_fwd as
add_fwd c (Nil) (Cons b bs)       = liftBitList (addBitU c (Cons b bs))
add_fwd c (Cons a as) (Cons b bs) =
    let (s, c') = add3k_fwd c a b
        r       = add_fwd c' as bs
    in Cons s r

add_bwd :: Bit -> LSB -> LSB -> LSB
add_bwd B0 Nil out = out
add_bwd B1 Nil out = inc_bwd out
add_bwd c (Cons b bs) out
    | eqBitList out (liftBitList (addBitU c (Cons b bs))) = Nil
    | Cons s r <- out =
        let (a, c') = add3k_bwd c b s
            as      = add_bwd c' bs r
        in Cons a as

isB0 :: Bit -> Bool
isB0 B0 = True
isB0 B1 = False

isB1 :: Bit -> Bool
isB1 B0 = False
isB1 B1 = True

fst :: (a, b) -> a
fst (a, b) = a

snd :: (a, b) -> b
snd (a, b) = b

(.) f g x = f (g x)

add3k = (add3k_fwd, add3k_bwd)

add3k_fwd :: Bit -> Bit -> Bit -> (Bit, Bit)
add3k_fwd B0 B0 B0 = (B0, B0)
add3k_fwd B0 B1 B0 = (B1, B0)
add3k_fwd B0 B0 B1 = (B1, B0)
add3k_fwd B0 B1 B1 = (B0, B1)
add3k_fwd B1 B0 B0 = (B1, B0)
add3k_fwd B1 B1 B0 = (B0, B1)
add3k_fwd B1 B0 B1 = (B0, B1)
add3k_fwd B1 B1 B1 = (B1, B1)

add3k_bwd :: Bit -> Bit -> Bit -> (Bit, Bit)
add3k_bwd B0 B0 B0 = (B0, B0)
add3k_bwd B0 B0 B1 = (B1, B0)
add3k_bwd B0 B1 B0 = (B1, B0)
add3k_bwd B0 B1 B1 = (B0, B1)
add3k_bwd B1 B0 B0 = (B1, B0)
add3k_bwd B1 B0 B1 = (B0, B1)
add3k_bwd B1 B1 B0 = (B0, B1)
add3k_bwd B1 B1 B1 = (B1, B1)

fwd :: (a, b) -> a
fwd h = case h of
          (x, y) -> x

bwd :: (a, b) -> b
bwd h = case h of
          (x, y) -> y

n0 = Nil
n1 = fwd inc n0
n2 = fwd inc n1
n3 = fwd inc n2
n4 = fwd inc n3
n5 = fwd inc n4
n6 = fwd inc n5
n7 = fwd inc n6

add43 = (fwd add) B0 n3 n4
sub73 = (bwd add) B0 n4 n7

add5 as = (fwd add) B0 as n5