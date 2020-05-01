-- 5. úloha
--
-- 1) Definujte datový typ 'Trie k v' reprezentující trii, kde klíče (řetězce)
-- jsou typu '[k]' a hodnoty typu 'v'.

data Trie k v = Node [(k, Trie k v)] (Maybe v) 
    deriving (Show,Eq)

-- Implementujte následující:

empty :: Trie k v
empty = Node [] Nothing

--
-- > empty == fromList []
--
{-
insert :: Int -> IntTree -> IntTree
insert x IntLeaf = IntNode x IntLeaf IntLeaf
insert x (IntNode y l r)
    | x <  y    = IntNode y (insert x l) r
    | x == y    = IntNode x l r
    | otherwise = IntNode y l (insert x r)
-}
singleton :: [k] -> v -> Trie k v
singleton [] v = Node [] (Just v)
singleton (x:xs) v = Node [(x,singleton xs v)] Nothing 

-- 'singleton ks v' je trie, která obsahuje právě jednen klíč 'ks'
-- s hodnotou 'v'.
--
-- > singleton ks v == fromList [(ks, v)]
--


insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith (f) [] new_v (Node [_] Nothing) = Node [] (Just new_v) 
insertWith (f) [] new_v (Node [_] (Just old_v)) = Node [] (Just (f new_v old_v)) 
--insertWith (f) (x:xs) new_v (Node sub_trees old_v) =
 --   case (lookup x sub_trees) of
  --      Just sub_tree   -> insertWith (f) xs new_v sub_tree
    --    Nothing         -> Node ((x,singleton xs new_v):sub_trees) old_v

insertWith (f) (x:xs) new_v (Node [] old_v) = Node [(x,singleton xs new_v)] old_v
insertWith (f) (x:xs) new_v (Node ((k,sub_trie):ns) old_v)
    | x == k    = Node ((k,insertWith (f) xs new_v sub_trie):ns) old_v
    | otherwise = Node ((k,sub_trie):(x,insertWith (f) (x:xs) new_v ns)) old_v
-- 'insertWith f ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud
-- trie již klíč 'ks' (s hodnotou 'old') obsahuje, původní hodnota je nahrazena
-- hodnotou 'f new old'.
--
-- > insertWith (++) "a" "x" empty                  == fromList [("a","x")]
-- > insertWith (++) "a" "x" (fromList [("a","y")]) == fromList [("a","xy")]
--

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert = undefined

-- 'insert ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud trie
-- již klíč 'ks' obsahuje, původní hodnota je nahrazena hodnotou 'new'
--
-- Hint: použijte 'insertWith'
--
-- > insert "a" "x" (fromList [("a","y")]) == fromList [("a","x")]
--

find :: (Ord k) => [k] -> Trie k v -> Maybe v
find = undefined

-- 'find k t' vrátí hodnotu odpovídající klíči 'k' (jako 'Just v'), pokud
-- existuje, jinak 'Nothing'.
--
-- > find "a" empty                  == Nothing
-- > find "a" (fromList [("a","x")]) == Just "x"
--

member :: (Ord k) => [k] -> Trie k v -> Bool
member = undefined

-- 'member k t' zjistí, jestli se klíč 'k' nalézá v trii 't'.
--
-- Hint: použijte 'find'
--
--
-- Funkce 'fromList' není nutná, ale může se vám hodit pro testování.

fromList :: (Ord k) => [([k], v)] -> Trie k v
fromList = undefined

-- BONUS) Implementujte funkci

delete :: (Ord k) => [k] -> Trie k v -> Trie k v
delete = undefined

-- 'delete ks t' smaže klíč 'ks' (a odpovídající hodnotu) z trie 't', pokud
-- klíč 'ks' není v trii obsažený, 'delete' vrátí původní trii.
--
-- > delete "a" (fromList [("b","y")]) == fromList [("b","y")]
-- > delete "a" (fromList [("a","x")]) == fromList []
