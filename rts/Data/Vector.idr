module Data.Vector

%default total
%access public export

export data Vector : Type -> Type

%extern prim__Vector_empty : Ptr
%extern prim__Vector_replicate : Nat -> Ptr -> Ptr
%extern prim__Vector_length : Ptr -> Nat
%extern prim__Vector_null : Ptr -> Bool
%extern prim__Vector_index : Ptr -> Nat -> Ptr
%extern prim__Vector_replaceAt : Nat -> Ptr -> Ptr -> Ptr
%extern prim__Vector_insertAt : Nat -> Ptr -> Ptr -> Ptr

||| O(1) Empty vector
empty : Vector a
empty = believe_me prim__Vector_empty

||| O(n) Vector of the given length with the same value in each position.
replicate : Nat -> a -> Vector a
replicate n x = believe_me (prim__Vector_replicate n (believe_me x))

||| O(1) Vector with exactly one element
singleton : a -> Vector a
singleton = replicate 1

||| O(1) Yield the length of the vector.
length : Vector a -> Nat
length xs = prim__Vector_length (believe_me xs)

||| O(1) Test whether a vector if empty
null : Vector a -> Bool
null xs = prim__Vector_null (believe_me xs)

infixl 6 !!

||| O(1) Indexing
(!!) : Vector a -> Nat -> a
(!!) xs i = believe_me (prim__Vector_index (believe_me xs) (believe_me i))

||| O(1) Safe Indexing
index' : Nat -> Vector a -> Maybe a
index' i xs = if (i < length xs) then Just (xs !! i) else Nothing

||| O(length xs) Insert new element.
unsafeInsertAt : Nat -> a -> Vector a -> Vector a
unsafeInsertAt n x xs = believe_me (prim__Vector_insertAt n (believe_me x) (believe_me xs))

||| O(length xs) Insert new element.
unsafeReplaceAt : Nat -> a -> Vector a -> Vector a
unsafeReplaceAt n x xs = believe_me (prim__Vector_replaceAt n (believe_me x) (believe_me xs))

||| O(length xs) Prepends an element.
(::) : a -> Vector a -> Vector a
(::) = unsafeInsertAt 0

||| O(1)
lastIndex : Vector a -> Maybe Nat
lastIndex xs =
  let
    n = length xs
  in
    case 1 `isLTE` n of
      Yes prf => Just (n - 1)
      _       => Nothing

elem : Eq a => a -> Vector a -> Bool
elem x xs =
  case length xs of
    Z   => False
    S k => go k
  where
    go : Nat -> Bool
    go Z = x == xs !! 0
    go i@(S k) = if x == xs !! i then True else go k

Foldable Vector where
  foldr f init xs =
    case length xs of
      Z   => init
      S k => go k init
    where
      go : Nat -> acc -> acc
      go Z       acc = f (xs !! 0) acc
      go i@(S k) acc = go k (f (xs !! i) acc)

  foldl f init xs =
    case length xs of
      Z   => init
      S k => go Z k init
    where
      go : Nat -> Nat -> acc -> acc
      go i Z     acc = f acc (xs !! i)
      go i (S k) acc = go (S i) k (f acc (xs !! i))

Show a => Show (Vector a) where
  show xs = foldl (\acc, e => (if (length acc > 1) then (acc ++ ", ") else acc) ++ show e) "[" xs ++ "]"
