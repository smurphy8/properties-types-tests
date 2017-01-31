{-# LANGUAGE ScopedTypeVariables #-}
module Example.Properties.Internal
    (
    ) where



{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-termination" @-}





{-@ type Nat   = {v:Int | 0 <= v}        @-}
{-@ type Even  = {v:Int | v mod 2 == 0 } @-}
{-@ type Lt100 = {v:Int | v < 100}       @-}



{-@ mapToEven :: Nat -> Even @-}
mapToEven (i::Int) = (2*i)


{-@ measure size @-}
{-@ size :: xs:[a] -> {v:Nat  | v = size xs }  @-}

size :: [a] -> Int
size [] = 0
size (_:rs) =  1 + size rs


{-@ measure notEmpty @-}

notEmpty [] = False
notEmpty _  = True



{-@ type NotEmptyList a = {xs:[a] | notEmpty xs } @-}


{-@ headSafe :: NotEmptyList a -> a @-}

headSafe (x:xs) = x



-- tryToUseHeadSafeUnSafely = headSafe []
