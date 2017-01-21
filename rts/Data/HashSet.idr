module Data.HashSet

import Data.Vector

%default total

public export
HashCode : Type
HashCode = Bits32

public export
interface (Eq a) => Hash a where
  hash : a -> HashCode

public export
Hash HashCode where
  hash = id

public export
Hash Int where
  hash = prim__zextInt_B32

public export
implementation (Hash a, Hash b) => Hash (a, b) where
  hash (x, y) = prim__xorB32 (997 * hash x) (991 * hash y)

Bitmap : Type
Bitmap = Bits32

Index : Type
Index = Nat

Cast Bits32 Nat where
  cast = cast . prim__zextB32_Int

mutual
  data Node a
    = Empty
    | Key HashCode a
    | HashCollision HashCode (Vector a)
    | SubTrie Bitmap (NodeList a)

  NodeList : Type -> Type
  NodeList a = Vector (Node a)

export
record HashSet a where
  constructor MkHashSet
  size : Nat
  root : NodeList a

Show a => Show (Node a) where
  show Empty = "E"
  show (Key h a) = "(K " ++ show a ++ ")"
  show (SubTrie bm l) = "(S " ++ show bm ++ " " ++ assert_total (show l) ++ ")"
  show _ = "*"

export
Show a => Show (HashSet a) where
  show (MkHashSet s r) = "HashSet " ++ show s ++ " " ++ show r

infixl 7 &
(&) : Bits32 -> Bits32 -> Bits32
(&) = prim__andB32

infixl 7 >>
(>>) : Bits32 -> Bits32 -> Bits32
(>>) = prim__lshrB32

infixl 7 <<
(<<) : Bits32 -> Bits32 -> Bits32
(<<) = prim__shlB32

(-) : Bits32 -> Bits32 -> Bits32
(-) x y = prim__zextInt_B32 (prim__zextB32_Int x - prim__zextB32_Int y)

shiftMask : Bits32 -> Bits32 -> Bits32 -> Bits32
shiftMask bits shift mask = (bits >> shift) & mask

popcnt : Bits32 -> Bits32
popcnt x =
  let x = x - shiftMask x 1 0x55555555
      x = (x & 0x33333333) + shiftMask x 2 0x33333333
      x = (x + (x >> 4)) & 0x0F0F0F0F
      x = x + (x >> 8)
      x = x + (x >> 16)
  in x & 0x0000003F

next5Bits : Bits32 -> HashCode -> HashCode
next5Bits level hashCode = shiftMask hashCode (5 * level) 0x1F

bitAt : Bits32 -> Bits32 -> Bool
bitAt pos bits = 1 == shiftMask bits pos 1

setBit : Bits32 -> Bits32 -> Bits32
setBit bitPos bits = prim__orB32 bits (1 << bitPos)

nodeIndex : Bits32 -> Bits32 -> Index
nodeIndex bitPos bitmap = cast (popcnt (bitmap & ((1 << bitPos) - 1)))

nodeAt : Index -> NodeList a -> Node a
nodeAt idx nodes = nodes !! idx

insertNode : Index -> NodeList a -> Node a -> NodeList a
insertNode idx nodes n = unsafeInsertAt idx n nodes

replaceNode : Index -> NodeList a -> Node a -> NodeList a
replaceNode idx nodes n = unsafeReplaceAt idx n nodes

{-
3.1 Search for a key Map Hash Array Mapped Trie (HAMT)

Compute a full 32 bit hash for the key, take the most significant t bits and use them as an integer to index into the root hash table. One of three cases may be encountered. First, the entry is empty indicating that the key is not in the hash tree. Second the entry is a Key/Value pair and the key either matches the desired key indicating success or not, indicating failure. Third, the entry has a 32 bit map sub-hash table and a sub-trie pointer, Base, that points to an ordered list of the non-empty sub-hash table entries.

Take the next 5 bits of the hash and use them as an integer to index into the bit Map. If this bit is a zero the hash table entry is empty indicating failure, otherwise, it’s a one, so count the one bits below it using CTPOP and use the result as the index into the non-empty entry list at Base. This process is repeated taking five more bits of the hash each time until a terminating key/value pair is found or the search fails. Typically, only a few iterations are required and it is important to note that the key is only compared once and that is with the terminating node key.
-}
export
member : (Hash a) => a -> HashSet a -> Bool
member key (MkHashSet _ root) =
  let rootPos = cast (next5Bits 0 hashCode)
  in memberOf root rootPos 1
  where
    hashCode : HashCode
    hashCode = hash key
    memberOf : NodeList a -> Index -> Bits32 -> Bool
    memberOf nodes index nextLevel =
      case nodeAt index nodes of
        Empty => False
        Key existingHashCode existingKey =>
          existingHashCode == hashCode && existingKey == key
        HashCollision existingHashCode keys =>
          existingHashCode == hashCode && elem key keys
        SubTrie bitmap nodes =>
          let bitPos = next5Bits nextLevel hashCode
              bitVal = bitAt bitPos bitmap
          in
            if bitVal
              then assert_total (memberOf nodes (nodeIndex bitPos bitmap) (nextLevel + 1))
              else False

{-
3.2 Insertion
The initial steps required to add a new key to the hash tree are identical to the search. The search algorithm is followed until one of two failure modes is encoun- tered.

Either an empty position is discovered in the hash table or a sub-hash table is found. In this case, if this is in the root hash table, the new key/value pair is simply substituted for the empty position. However, if in a sub-hash table then a new bit must be added to the bit map and the sub-hash table increased by one in size. A new sub-hash table must be allocated, the existing sub-table copied to it, the new key/value entry added in sub-hash sorted order and the old hash table made free.

Or the key will collide with an existing one. In which case the existing key must be replaced with a sub-hash table and the next 5 bit hash of the existing key computed. If there is still a collision then this process is repeated until no collision occurs. The existing key is then inserted in the new sub-hash table and the new key added. Each time 5 more bits of the hash are used the probability of a collision reduces by a factor of 1 . Occasionally an entire 32 bit hash may be consumed and 32 a new one must be computed to differentiate the two keys.
-}
insert' : (Hash a) => HashCode -> a -> HashSet a -> HashSet a
insert' hashCode key set@(MkHashSet size root) =
   case nodeAt rootPos root of
     Empty => insertRootNode (Key hashCode key)
     SubTrie bitmap nodes =>
       maybe set insertRootNode (insertIntoSubTrie bitmap nodes 1)
     node@(Key existingHashCode existingKey) =>
       if existingHashCode == hashCode
         then
           if existingKey == key
             then set
             else insertRootNode (collision existingKey)
         else
           maybe set insertRootNode (spawnSubTrie node existingHashCode 1)
     node@(HashCollision existingHashCode keys) =>
       if existingHashCode == hashCode
         then
           if elem key keys
             then set
             else insertRootNode (HashCollision hashCode (key :: keys))
         else
           maybe set insertRootNode (spawnSubTrie node existingHashCode 1)
  where
    rootPos : Index
    rootPos = cast (next5Bits 0 hashCode)
    insertRootNode : Node a -> HashSet a
    insertRootNode = MkHashSet (S size) . replaceNode rootPos root
    collision : a -> Node a
    collision existingKey = HashCollision hashCode (key :: singleton existingKey)
    mutual
      insertIntoSubTrie : Bitmap -> NodeList a -> Bits32 -> Maybe (Node a)
      insertIntoSubTrie bitmap nodes level =
        let
          bitPos = next5Bits level hashCode
          bitVal = bitAt bitPos bitmap
          nodePos = nodeIndex bitPos bitmap
        in
          if bitVal
            then SubTrie bitmap . replaceNode nodePos nodes <$> insertInto (nodeAt nodePos nodes) level
            else Just (SubTrie (setBit bitPos bitmap) (insertNode nodePos nodes (Key hashCode key)))

      insertInto : Node a -> Bits32 -> Maybe (Node a)
      insertInto (SubTrie bitmap nodes) level =
        assert_total (insertIntoSubTrie bitmap nodes (level + 1))
      insertInto node@(Key existingHashCode existingKey) level =
        if existingHashCode == hashCode
          then
            if existingKey == key
              then Nothing
              else Just (collision existingKey)
          else
            assert_total (spawnSubTrie node existingHashCode (level + 1))
      insertInto node@(HashCollision existingHashCode keys) level =
        if existingHashCode == hashCode
          then
            if elem key keys
              then Nothing
              else Just (HashCollision hashCode (key :: keys))
          else
            assert_total (spawnSubTrie node existingHashCode (level + 1))
      insertInto Empty _ = assert_unreachable

      spawnSubTrie : Node a -> HashCode -> Bits32 -> Maybe (Node a)
      spawnSubTrie node existingHashCode level =
        let bitPos = next5Bits level existingHashCode
            bitmap = setBit bitPos 0
        in insertIntoSubTrie bitmap (singleton node) level

export
insert : (Hash a) => a -> HashSet a -> HashSet a
insert key set = insert' (hash key) key set

export
empty : HashSet a
empty = MkHashSet 0 (replicate 32 Empty)

export
length : HashSet a -> Nat
length (MkHashSet size _) = size

fold : (acc -> elem -> acc) -> acc -> HashSet elem -> acc
fold f init (MkHashSet _ nodes) = foldNodes init nodes
  where
   mutual
    foldNodes : acc -> NodeList elem -> acc
    foldNodes acc nodes = foldl foldNode acc nodes
    foldNode : acc -> Node elem -> acc
    foldNode acc Empty = acc
    foldNode acc (Key _ k) = f acc k
    foldNode acc (HashCollision _ keys) = foldl f acc keys
    foldNode acc (SubTrie _ nodes) = assert_total (foldNodes acc nodes)

export
Foldable HashSet where
  foldl = fold
  foldr = fold . flip

export
filter : (Eq elem, Hash elem) => (elem -> Bool) -> HashSet elem -> HashSet elem
filter f (MkHashSet _ nodes) = foldNodes empty nodes
  where
   mutual
    foldNodes : HashSet elem -> NodeList elem -> HashSet elem
    foldNodes acc nodes = foldl foldNode acc nodes
    foldNode : HashSet elem -> Node elem -> HashSet elem
    foldNode acc Empty = acc
    foldNode acc (Key hashCode k) = insertWithHashCode hashCode acc k
    foldNode acc (HashCollision hashCode keys) = foldl (insertWithHashCode hashCode) acc keys
    foldNode acc (SubTrie _ nodes) = assert_total (foldNodes acc nodes)
    insertWithHashCode : HashCode -> HashSet elem -> elem -> HashSet elem
    insertWithHashCode hc acc e = if f e then insert' hc e acc else acc

export
into : (Hash a, Foldable f) => HashSet a -> f a -> HashSet a
into = foldl (flip insert)

export
fromList : (Hash a) => List a -> HashSet a
fromList = foldl (flip insert) empty

export
foldM : Monad m => (acc -> elem -> m acc) -> acc -> HashSet elem -> m acc
foldM f acc (MkHashSet _ nodes) = foldNodes f acc nodes
  where
   mutual
    foldNodes : (Monad m) => (a -> elem -> m a) -> a -> NodeList elem -> m a
    foldNodes f acc nodes = loop (length nodes) acc
      where
        loop Z     acc = pure acc
        loop (S k) acc = foldNode f acc (nodes !! k) >>= loop k
    foldNode : (Monad m) => (a -> elem -> m a) -> a -> Node elem -> m a
    foldNode f acc Empty = pure acc
    foldNode f acc (Key _ k) = f acc k
    foldNode f acc (HashCollision _ keys) = foldKeys f acc keys
    foldNode f acc (SubTrie _ nodes) = assert_total (foldNodes f acc nodes)
    foldKeys : (Monad m) => (a -> elem -> m a) -> a -> Vector elem -> m a
    foldKeys f acc keys = loop (length keys) acc
      where
        loop Z     acc = pure acc
        loop (S k) acc = f acc (keys !! k) >>= loop k
