module Data.Nullable

%default total

%access public export

||| A foreign reference that is allowed to have a null value.
export data Nullable : Type -> Type

%inline
null : Nullable a
null = believe_me prim__null

%inline
isNull : Nullable a -> Bool
isNull n = the Ptr (believe_me n) == null

%inline
nullable : (Lazy b) -> Lazy (a -> b) -> Nullable a -> b
nullable d f n =
  if isNull n
    then d
    else f (believe_me n)

%inline
asNullable : a -> Nullable a
asNullable a = believe_me a

%inline
notNull : Nullable a -> Maybe a
notNull = nullable Nothing Just

Functor Nullable where
  map f = nullable null (asNullable . f)
