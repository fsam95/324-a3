{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA
    )
    where


type AList a b = [(a, b)]

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA alist key = 
  if ((fst (head alist)) == key)
     then (snd (head alist))
     else lookupA (tail alist) key

-- | Returns a new association list which is the old one, except with 
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA alist (key, val) = 
  if (containsA alist key)
     then alist
     else alist ++ [(key,val)]

-- | Returns a new association list which is the old one, except with 
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA alist (key, val) = 
  updateHelper alist (key, val) []
                   

-- | Returns true if the given  key is contained in the association list
--   Otherwise returns false
containsA :: Eq a => AList a b -> a -> Bool
containsA alist key  
  | null alist = False
  | otherwise = if ((fst (head alist)) == key)
                   then True
                   else containsA (tail alist) key

updateHelper alist (key, val) acc
  | null alist = acc
  | otherwise = if ((fst (head alist)) == key)
                   then updateHelper (tail alist) (key, val) (acc ++ [(key,val)])
                   else updateHelper (tail alist) (key, val) (acc ++ [(head alist)])
