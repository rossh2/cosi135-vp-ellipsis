module Model where 

import Data.List

data Entity = A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
     deriving (Eq,Show,Bounded,Enum)

entities :: [Entity]
entities =  [minBound..maxBound] 

snowWhite, alice, dorothy, goldilocks, littleMook, atreyu :: Entity

snowWhite  = S
alice      = A
dorothy    = D
goldilocks = G 
littleMook = M
atreyu     = Y

data World = W1 | W2 | W3 | W4
    deriving (Eq, Show, Bounded, Enum)

worlds :: [World]
worlds =  [minBound..maxBound]

-- Accessibility relation between worlds, here, only use will be for time
type AccRel = World -> [World]

present :: AccRel
present w = [w]

past :: AccRel
-- all worlds before but not including w
past w = init (enumFromTo minBound w)

perfect :: AccRel
-- for this model there is no difference between past and perfect, both are previous worlds
perfect = past

pastPerfect :: AccRel
-- all worlds before but not including w or the immediate past
pastPerfect w = (init' . init) (enumFromTo minBound w)
    where
        init' [] = []
        init' xs = init xs

future :: AccRel
-- all worlds after but not including w (true future, does not include current world)
future w = tail (enumFrom w)

futurePerfect :: AccRel
-- all worlds in the past of some future world (=all worlds except W4, provided the world has a future)
futurePerfect w
    | w == maxBound = []
    | otherwise     = init worlds

pastFuture :: AccRel
-- all worlds in the future of some past world (=all worlds except W1, provided world has a past)
pastFuture w
    | w == minBound = []
    | otherwise     = tail worlds

type OnePlacePred   = World -> Entity -> Bool
type TwoPlacePred   = World -> Entity -> Entity -> Bool

list2OnePlacePred :: [Entity] -> OnePlacePred
list2OnePlacePred xs = \w x -> elem x xs

list2TwoPlacePred :: [(Entity, Entity)] -> TwoPlacePred
list2TwoPlacePred xs = \w -> curry (`elem` xs)

worldLists2OnePlacePred :: [(World, [Entity])] -> OnePlacePred
worldLists2OnePlacePred wxs = \w x -> (w, x) `elem` flatList
    where flatList = distribute wxs

worldLists2TwoPlacePred :: [(World, [(Entity, Entity)])] -> TwoPlacePred
worldLists2TwoPlacePred wxs = \w x y -> (w, (x, y)) `elem` flatList
    where flatList = distribute wxs

distribute :: [(a, [b])] -> [(a, b)]
distribute xs = xs >>= \tup -> map (\e -> (fst tup, e)) (snd tup)

girl, boy, princess, dwarf, giant, wizard, sword, dagger :: OnePlacePred

-- Assume our common nouns are the same in all worlds
girl     = list2OnePlacePred [S,A,D,G]
boy      = list2OnePlacePred [M,Y]
princess = list2OnePlacePred [E]
dwarf    = list2OnePlacePred [B,R]
giant    = list2OnePlacePred [T]
wizard   = list2OnePlacePred [W,V]
sword    = list2OnePlacePred [F,C,H]
dagger   = list2OnePlacePred [X]

child, person, man, woman, male, female, thing :: OnePlacePred

child  = \ w x -> (girl w x  || boy w x)
person = \ w x -> (child w x || princess w x || dwarf w x
                         || giant w x || wizard w x)
man    = \ w x -> (dwarf w x || giant w x || wizard w x)
woman  = \ w x -> princess w x
male   = \ w x -> (man w x || boy w x)
female = \ w x -> (woman w x || girl w x)
thing  = \ w x -> not (person w x || x == Unspec)

laugh, cheer, shudder :: OnePlacePred

laugh   = worldLists2OnePlacePred [(W1, [A,G,E,W]), (W2, [G,E,B]), (W3, [V,M,Y]), (W4, [G])]
cheer   = worldLists2OnePlacePred [(W1, [M,D,A]), (W2, [M,A]), (W4, [M,D,A])]
shudder = worldLists2OnePlacePred [(W1, [S,Y]), (W3, [V])]

love, admire, help, defeat, possess :: TwoPlacePred

-- n.B. tuples are (object, subject)
love = worldLists2TwoPlacePred [
    (W1, [(Y,E),(B,S),(R,S),(F,Y),(C,M),(X,E),(F,S)]),
    (W2, [(B,S),(R,S)]),
    (W3, [(B,S),(R,S),(S,B),(S,R),(F,Y),(C,M),(X,E)]),
    (W4, [(Y,E),(B,S),(R,S),(S,B),(S,R),(F,Y),(C,M),(X,E)])
    ]
admire = worldLists2TwoPlacePred [
    (W1, [(x,G) | x <- entities, person W1 x] ++ [(D, Y)]),
    (W2, [(x,G) | x <- entities, person W1 x] ++ [(D, Y)]),
    (W3, [(x,G) | x <- entities, person W1 x] ++ [(Y, E)]),
    (W4, [(x,G) | x <- entities, person W1 x])
    ]
help   = worldLists2TwoPlacePred [
    (W1, [(W,W),(V,V),(S,B),(D,M)]),
    (W2, [(B,S), (R,S)]),
    (W3, [(S,B), (S,R)]),
    (W4, [(D,D)])
    ]
defeat = worldLists2TwoPlacePred [
    (W1, [(x,y) | x <- entities,
                  y <- entities,
                  dwarf W1 x && giant W1 y]
         ++ [(A,W),(A,V),(W,Y),(W,F)]),
    (W4, [(W, A), (V, A)])
    ]
-- Assume ownership / possessive pronouns the same in all worlds
possess = list2TwoPlacePred [(X,E), (F,Y), (C,M), (H,S)]
