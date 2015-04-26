{-
 - AVL tree in haskell (insert and select)
 -}

data  AVL a = Null | Branch a (AVL a) (AVL a) Int Int -- height size
            deriving Show

height :: AVL a -> Int
height Null = 0
height (Branch _ _ _ h _) = h

size :: AVL a -> Int
size Null = 0
size (Branch _ _ _ _ s) = s

insert :: (Ord a) => AVL a -> a -> AVL a
insert Null val = Branch val Null Null 1 1
insert (Branch this lt rt h s) val =
        balance (if val < this then Branch this (insert lt val) rt h (s+1)
                               else Branch this lt (insert rt val) h (s+1))

rol :: AVL a -> AVL a
rol (Branch x lt (Branch y rlt rrt _ _) _ s) =
        Branch y (Branch x lt rlt (1 + max (height lt) (height rlt))
                                  (1 + size lt + size rlt))
                 rrt
                 (1 + max (1 + max (height lt) (height rlt)) (height rrt)) s

ror :: AVL a -> AVL a
ror (Branch x (Branch y llt lrt _ _) rt _ s) =
        Branch y llt
                 (Branch x lrt rt (1 + max (height lrt) (height rt))
                                  (1 + size lrt + size rt))
                 (1 + max (height llt) (1 + max (height lrt) (height rt))) s

balance :: AVL a -> AVL a
balance self@(Branch this lt rt h s)
    | height lt - height rt >  1 =
        case lt of Null -> ror self
                   (Branch _ llt lrt _ _) ->
                       let lt' = if height llt < height lrt then rol lt else lt
                       in  ror (Branch this lt' rt h s)
    | height lt - height rt < -1 =
        case rt of Null -> rol self
                   (Branch _ rlt rrt _ _) ->
                       let rt' = if height rlt > height rrt then ror rt else rt
                       in  rol (Branch this lt rt' h s)
    | otherwise = Branch this lt rt (1 + max (height lt) (height rt)) s

select :: AVL a -> Int -> a
select (Branch this lt rt _ _) k
    | rank == k = this
    | rank >  k = select lt k
    | otherwise = select rt (k - rank)
     where rank = size lt + 1

main :: IO ()
main = do
        let tree@(Branch _ lt rt _ _) = foldl insert Null [1..100000]
        putStrLn $ "(height, left height, right height): " ++ show (height tree, height lt, height rt)
        putStrLn $ "test select: " ++ show (and [i == select tree i | i <- [1 .. 100000]])
