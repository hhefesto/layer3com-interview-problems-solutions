Interview Programming Problems Solutions
Layer 3 Communication

First we must define our extensions, the module and our imports.
\begin{code}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE InstanceSigs                #-}

module Solutions where

import Data.Word8 (Word8)
import qualified Data.Vector as V
import Text.Read (readMaybe)
import Control.Applicative

\end{code}

1. Insertion into a binary tree

Lets start by defining our tree
\begin{code}
data Bin a
  = Leaf
  | Node a (Bin a) (Bin a)
  deriving Show
\end{code}

Which is pretty straight forward.

As it is natural with trees, our insertion will be recursive.

The base case would be to try to insert into a tree's Leaf, which resolves to
the same tree, but with a Node with two extra Leafs instead of the previously
reached Leaf.

The recursive case would be to try to insert into a node, which has two possible
outcomes: insert to the left or insert to the right.

We will order our tree such that at any given Node, the values in the left child
are smaller or equal than the node's value, and the values in the right child
are greater or equal that the node's value.

In the case of the inserted value being equal to the Node's value we will choose
to insert into the shallowest child or to the left child in case of equal
depths. This will certainly not keep the tree balanced, but at least it won't
worsen the balance state.

\begin{code}
insert :: Ord a => a -> Bin a -> Bin a
insert x Leaf = Node x Leaf Leaf
insert x (Node y lc rc)
  | x < y = Node y (insert x lc) rc
  | x > y = Node y lc (insert x rc)
  | x == y = case depth lc <= depth rc of
               True -> Node y (insert x lc) rc
               False -> Node y lc (insert x rc)
    where
      depth :: Bin a -> Integer
      depth Leaf = 0
      depth (Node _ Leaf Leaf) = 1
      depth (Node _ l Leaf) = 1 + depth l
      depth (Node _ Leaf r) = 1 + depth r
      depth (Node _ l r) = max (depth l) (depth r)
\end{code}

2. IPv4 Permutations

We will start by defining a parser a la Graham Hutton's Programming in Haskell
\begin{code}
newtype Parser a = P ([Char] -> [(a, [Char])])
  deriving (Functor)

parse :: Parser a -> [Char] -> [(a, [Char])]
parse (P f) str = f str

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P (\input -> [(x, input)])
  
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  f <*> p = P (\input -> case parse f input of
                           [] -> []
                           [(g, rest)] -> parse (fmap g p) rest)

\end{code}

So far, the parser is just like Hutton's, but we have to make a slight
modification on the Monad instance, because Hutton manages a succesful parse as
a singleton list, and we do not want that.

In our case, a successful parse of a Word8 could output several results (e.g. we
want "111" to be parsed in three different ways:
  (1, "11"), (11, "1"), (111, "")),
and the Hutton's bind only takes into account a singleton list. So our bind
should also manage the rest of the list, and that is what the last case of bind
does.

\begin{code}

instance Monad Parser where
  return :: a -> Parser a
  return = pure
  
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\input -> case parse p input of
                           [] -> []
                           [(x, rest)] -> parse (f x) rest
                           x -> concatMap aux x
                             where
                               aux pair = parse (f $ fst pair) (snd pair))

\end{code}

So now we need to parse Word8 with this multiple interpretations style.

Notice that a Word8 cannot have 4 digits because anything that has 4 digits with
a non-zero start will be greater than 255, so we use pattern matching to consume
1-3 digits and in each case give a possible interpretation.

\begin{code}

word8 :: Parser Word8
word8 = P (\input -> case input of
                       [] -> []
                       (x1:[]) -> [(read [x1], [])]
                       x@(x1:x2:[]) -> [ (read [x1], [x2])
                                       , (read x, [])]
                       x@(x1:x2:x3:[]) -> [ (read [x1], [x2,x3])
                                          , (read [x1, x2], [x3])
                                          , (read x, []) ]
                       (x1:x2:x3:xs) -> [ (read [x1], [x2,x3] ++ xs)
                                        , (read [x1, x2], [x3] ++ xs)
                                        , (read [x1, x2, x3], xs) ])

\end{code}

Lastly, we define our helper type IPv4 and our ipv4 parser that simply outputs
the result of sequencing four word8 parsers

\begin{code}

type IPv4 = (Word8, Word8, Word8, Word8)

ipv4 :: Parser IPv4
ipv4 = do
  x1 <- word8
  x2 <- word8
  x3 <- word8
  x4 <- word8
  pure (x1,x2,x3,x4)

\end{code}

Finally "parse ipv4 input" is all we need to generate our possible addresses.

To only show a list of [IPv4] we filter out any tuple that didn't completely
consume its input and only take the first element of the tuples.

\begin{code}

possibleAddresses :: String -> [IPv4]
possibleAddresses input = fmap fst $ filter (\x -> length (snd x) == 0) $ parse ipv4 input
  
  
\end{code}


3. Missing number

A naive way of doing this problem would be to order the list and scan for the
missing number by traversing the list, but that would be of time complexity
O(n log n) because of the sorting.

To do a O(n) implementation, we will be using Vector thanks to its ability to
modify one of its entries in O(1)

So the strategy is to first create a Vector of size n with with the (index + 1)
in each entry (e.g. [1,2,3,4]).

This "ordered" vector will be sort of our "attendance list".

While we traverse our unordered list (our input) we will be
"checking assistance" by turning to zero the corresponding entry in the ordered
vector.

e.g. input = [4,1,2]
     ordered = [1,2,3,4]

     and after consuming the first element of our input, our vectors would be:
     input = [1,2]
     ordered = [1,2,3,0]

When all the input is consumed, the ordered vector will be filled with zeros
except for the missing number (the one that didn't take attendance because it
wasn't there). So we fold that vector with sum (O(n)) and the result is going
to be our missing number.

But this was all done with Vectors, and the problem indicates that our input is
a list, but we can easily transform it to a vector in O(n) with V.fromList.
q.e.d.

\begin{code}

missingNumber' :: V.Vector Int -> V.Vector Int -> Int
missingNumber' ordered input = case V.length input of
                                 0 -> V.foldl' (+) 0 ordered
                                 _ -> missingNumber' o i
                                   where i = V.tail input
                                         pos = (V.head input) - 1
                                         o = ordered V.// [(pos, 0)]

missingNumber :: [Int] -> Int
missingNumber input = missingNumber' ordered (V.fromList input)
  where ordered = V.generate (length input + 1) (\x -> x + 1)

\end{code}
