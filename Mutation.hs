{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer 
    )
    where


import AList (AList, lookupA, insertA, updateA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show 


-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Memory -> Pointer a -> a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Memory -> Pointer a -> a -> Memory

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Memory -> Integer -> a -> (Pointer a, Memory)

instance Mutable Integer where
  get memory (P num) = 
    let value = lookupA memory num
     in case value of IntVal num -> num


  set memory (P num) newVal = 
    updateA memory (num, IntVal newVal)
  
  def memory location value =
    (P location, insertA memory (location, IntVal value))

instance Mutable Bool where
  get memory (P num) = 
    let value = lookupA memory num
     in case value of BoolVal bool -> bool
    --    let value = lookupA memory num
    --   IntVal 

  set memory (P num) newVal = 
    updateA memory (num, BoolVal newVal)
  
  def memory location value =
    (P location, insertA memory (location, BoolVal value))

