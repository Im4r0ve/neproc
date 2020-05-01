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

singleton :: [k] -> v -> Trie k v
singleton [] v = Node [] (Just v)
singleton (x:xs) v = Node [(x,singleton xs v)] Nothing 

-- 'singleton ks v' je trie, která obsahuje právě jednen klíč 'ks'
-- s hodnotou 'v'.
--
-- > singleton ks v == fromList [(ks, v)]
--
hasEdge :: (Ord k) => k -> [(k,Trie k v)] -> Bool
hasEdge _ [] = False
hasEdge k ((k', _ ):edges) 
    | k == k'   = True
    | otherwise = hasEdge k edges

insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith _ [] new_v (Node sub_tries Nothing) = Node sub_tries (Just new_v) 
insertWith (f) [] new_v (Node sub_tries (Just old_v)) = Node sub_tries (Just (f new_v old_v)) 
insertWith (f) (k:ks) new_v (Node sub_tries old_v)
    | hasEdge k sub_tries = Node (map (\(k',sub_trie) -> if (k == k') then (k,insertWith (f) ks new_v sub_trie) else (k',sub_trie)) sub_tries) old_v
    | otherwise           = Node ((k,singleton ks new_v):sub_tries) old_v
{-
replaceTrie 
insertWith (f) (x:xs) new_v (Node [] old_v) = Node [(x,singleton xs new_v)] old_v
insertWith (f) (x:xs) new_v (Node ((k,sub_trie):(k2,sub_trie2):ns) old_v)
    | x == k    = Node ((k,insertWith (f) (xs) new_v sub_trie):ns) old_v
    | otherwise = Node ((k,sub_trie):(k2,insertWith (f) (x:xs) new_v sub_trie2):ns) old_v -- posunie sa dalej-}
    -- | x == k    = Node ((k,insertWith (f) xs new_v sub_trie):ns) old_v
    -- Node (k,insertWith (f) xs new_v sub_trie) old_v -- nahradi match rekurzivnym zavolanim

-- 'insertWith f ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud
-- trie již klíč 'ks' (s hodnotou 'old') obsahuje, původní hodnota je nahrazena
-- hodnotou 'f new old'.
--
-- > insertWith (++) "a" "x" empty                  == fromList [("a","x")]
-- > insertWith (++) "a" "x" (fromList [("a","y")]) == fromList [("a","xy")]
-- > insertWith (++) "abd" "y" Node [('a',Node [('b',Node [('c',Node [] (Just "x"))] Nothing)] Nothing)] Nothing
replace :: v -> v -> v
replace _ new = new

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert k = insertWith (replace) k
-- insert k new_v (Node sub_tries Nothing) = insertWith (replace) k (Just new_v) (Node sub_tries Nothing)

-- 'insert ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud trie
-- již klíč 'ks' obsahuje, původní hodnota je nahrazena hodnotou 'new'
--
-- Hint: použijte 'insertWith'
--
-- > insert "a" "x" (fromList [("a","y")]) == fromList [("a","x")]
--

find :: (Ord k) => [k] -> Trie k v -> Maybe v
find _ (Node [] v) = v
find [] (Node _ v) = v 
find (k:ks) (Node sub_tries _) = case (lookup k sub_tries) of
    Just sub_trie   -> find ks sub_trie
    Nothing         -> Nothing

-- 'find k t' vrátí hodnotu odpovídající klíči 'k' (jako 'Just v'), pokud
-- existuje, jinak 'Nothing'.
--
-- > find "a" empty                  == Nothing
-- > find "a" (fromList [("a","x")]) == Just "x"
--

member :: (Ord k) => [k] -> Trie k v -> Bool
member k trie = case (find k trie) of
    Just _ -> True
    Nothing -> False

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
