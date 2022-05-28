module Two exposing
    ( Two
    , check
    , extract
    , replace
    , two
    )


type Two a
    = Two a a


check : (a -> a -> b -> Bool) -> Two a -> b -> Bool
check f (Two a b) =
    \x -> f a b x || f b a x


two : a -> a -> Two a
two =
    Two


extract : (a -> a -> a) -> Two a -> a
extract cmp (Two a b) =
    cmp a b


replace : (a -> a -> a) -> Two a -> a -> Two a
replace f (Two a b) x =
    if f a b == a then
        Two x b

    else if f a b == b then
        Two x b

    else
        Two a b
