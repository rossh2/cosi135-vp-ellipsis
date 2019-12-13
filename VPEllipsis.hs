module VPEllipsis where

import Grammar
import Model

type PlainVP = AccRel -> NounPhrase -> Sentence
data Context = Context {contextEntities :: [Entity], vps :: [PlainVP]}

type Sentence = Context -> (Context -> Bool) -> World -> Bool
type Discourse = Sentence

type Noun = Entity -> Sentence
type NounPhrase = (Entity -> Sentence) -> Sentence

addEntity :: Entity -> Context -> Context
addEntity e (Context entities vps) = Context (e : entities) vps

addVP :: PlainVP -> Context -> Context
addVP vp (Context entities vps) = Context entities (vp : vps)

liftNP :: Entity -> NounPhrase
liftNP e = \p i k w -> (p e) (addEntity e i) k w

liftVP :: OnePlacePred -> AccRel -> NounPhrase -> Sentence
liftVP iv accRel = \np -> np (\x i k w -> any (\w' -> iv w' x) (accRel w) && k i)

liftTV :: TwoPlacePred -> AccRel -> NounPhrase -> NounPhrase -> Sentence
liftTV tv accRel = \objNP subjNP -> subjNP (\x -> objNP (\y i k w -> any (\w' -> tv w' y x) (accRel w) && k i))

liftCN :: OnePlacePred -> Noun
liftCN cn = \x i k w -> cn w x && k i

intSent :: Sent -> Sentence
intSent (Sent np vp) = (intVP vp) (intNP np)

intDET :: DET -> Noun -> NounPhrase
intDET Some = \p q i k w -> any (\x -> (p x) i (\i' -> (q x) (addEntity x i') k w) w) entities
intDET Every = \p q i k w -> all (\x -> not ((p x) i (\i' -> not ((q x) (addEntity x i') trivial w)) w)) entities && k i
    where trivial = \i'' -> True

intDET His = intPossPronoun
intDET Her = intPossPronoun
intDET Its = intPossPronoun

intPossPronoun :: Noun -> NounPhrase
intPossPronoun = \p q i k w -> let owner = sel i
                                   matching = filter (\x -> (p x) i k w && (possess w x owner)) entities
                                   applied = any (\x -> (p x) i (\i' -> (q x) (addEntity x i') k w) w) matching
                               in length matching == 1 && applied

intPronoun :: NounPhrase
intPronoun = \p i k w -> (p (sel i)) i k w

sel :: Context -> Entity
sel (Context entities preds) = head entities

selVP :: Context -> PlainVP
selVP (Context entities vps) = head vps

intNP :: NP -> NounPhrase
intNP SnowWhite  = liftNP snowWhite
intNP Alice      = liftNP alice
intNP Dorothy    = liftNP dorothy
intNP Goldilocks = liftNP goldilocks
intNP LittleMook = liftNP littleMook
intNP Atreyu     = liftNP atreyu

intNP Someone  = (intDET Some)  (liftCN person)
intNP Everyone = (intDET Every) (liftCN person)

intNP She  = intPronoun
intNP He   = intPronoun
intNP It   = intPronoun

intNP (NP1 det cn) = (intDET det) (intCN cn)

liftEllipsis :: AccRel -> NounPhrase -> Sentence
liftEllipsis accRel = \np i k w -> ((selVP i) accRel) np i k w

liftAndAdd :: (AccRel -> NounPhrase -> Sentence) -> AccRel -> NounPhrase -> Sentence
liftAndAdd boundLift accRel = \np i k w -> (boundLift accRel) np (addVP boundLift i) k w

liftAndAddOnePlace :: OnePlacePred -> AccRel -> NounPhrase -> Sentence
liftAndAddOnePlace v accRel = liftAndAdd (\t -> liftVP v t) accRel

liftAndAddTwoPlace :: TwoPlacePred -> AccRel -> NP -> NounPhrase -> Sentence
liftAndAddTwoPlace tv accRel objNP = liftAndAdd (\t -> (liftTV tv t) (intNP objNP)) accRel

intVP :: VP -> NounPhrase -> Sentence
intVP Laughs             = liftAndAddOnePlace laugh present
intVP Laughed            = liftAndAddOnePlace laugh past
intVP Cheers             = liftAndAddOnePlace cheer present
intVP Cheered            = liftAndAddOnePlace cheer past
intVP Shudders           = liftAndAddOnePlace shudder present
intVP Shuddered          = liftAndAddOnePlace shudder past
intVP (VP1 Loves np)     = liftAndAddTwoPlace love present np
intVP (VP1 Loved np)     = liftAndAddTwoPlace love past np
intVP (VP1 Admires np)   = liftAndAddTwoPlace admire present np
intVP (VP1 Admired np)   = liftAndAddTwoPlace admire past np
intVP (VP1 Helps np)     = liftAndAddTwoPlace help present np
intVP (VP1 Helped np)    = liftAndAddTwoPlace help past np
intVP (VP1 Defeats np)   = liftAndAddTwoPlace defeat present np
intVP (VP1 Defeated np)  = liftAndAddTwoPlace defeat past np
intVP (VP5 Will inf)     = intInf inf future
intVP (VP5 Would inf)    = intInf inf pastFuture
intVP (VP5 Does inf)     = intInf inf present
intVP (VP5 Did inf)      = intInf inf past
intVP (VP6 Has en)       = intEn en perfect
intVP (VP6 Had en)       = intEn en pastPerfect
intVP (VP4 Does Too)     = liftEllipsis present
intVP (VP4 Did Too)      = liftEllipsis past
intVP (VP4 Will Too)     = liftEllipsis future
intVP (VP4 Would Too)    = liftEllipsis pastFuture
intVP (VP7 Has Too)      = liftEllipsis past
intVP (VP7 Had Too)      = liftEllipsis pastPerfect

intInf :: INF -> AccRel -> NounPhrase -> Sentence
intInf Laugh           accRel = liftAndAddOnePlace laugh accRel
intInf Cheer           accRel = liftAndAddOnePlace cheer accRel
intInf Shudder         accRel = liftAndAddOnePlace shudder accRel
intInf (INF Love np)   accRel = liftAndAddTwoPlace love accRel np
intInf (INF Admire np) accRel = liftAndAddTwoPlace admire accRel np
intInf (INF Help np)   accRel = liftAndAddTwoPlace help accRel np
intInf (INF Defeat np) accRel = liftAndAddTwoPlace defeat accRel np

intEn :: EN -> AccRel -> NounPhrase -> Sentence
intEn LaughedEN           accRel = liftAndAddOnePlace laugh accRel
intEn CheeredEN           accRel = liftAndAddOnePlace cheer accRel
intEn ShudderedEN         accRel = liftAndAddOnePlace shudder accRel
intEn (EN LovedEN np)     accRel = liftAndAddTwoPlace love accRel np
intEn (EN AdmiredEN np)   accRel = liftAndAddTwoPlace admire accRel np
intEn (EN HelpedEN np)    accRel = liftAndAddTwoPlace help accRel np
intEn (EN DefeatedEN np)  accRel = liftAndAddTwoPlace defeat accRel np

intCN :: CN -> Noun
intCN Girl     = liftCN girl
intCN Boy      = liftCN boy
intCN Princess = liftCN princess
intCN Dwarf    = liftCN dwarf
intCN Giant    = liftCN giant
intCN Sword    = liftCN sword
intCN Dagger   = liftCN dagger
intCN Wizard   = liftCN wizard

compose :: Discourse -> Sentence -> Discourse
d `compose` s = \i k w -> d i (\i' -> s i' k w) w

compSent :: Sent -> World -> Bool
compSent s w = (intSent s) (Context [] []) trivial w
    where trivial = \i'' -> True

compText :: [Sent] -> World -> Bool
compText t w = composed (Context [] []) trivial w
    where trivial = \i'' -> True
          composed = foldl (\d s -> d `compose` (intSent s)) (intSent (head t) :: Discourse) (tail t)

examples = [
    (compSent (Sent Everyone Laughed) W2, False),
    (compSent (Sent Someone Laughed) W2, True),
    (compSent (Sent Everyone (VP1 Admired Goldilocks)) W2, False),
    (compSent (Sent Goldilocks (VP1 Admired Everyone)) W2, True),
    (compSent (Sent Goldilocks (VP1 Admired Someone)) W2, True),
    (compSent (Sent Goldilocks (VP1 Admired Someone)) W2, True),
    (compSent (Sent (NP1 Some Boy) Laughed) W2, False)
    ]

tenseAspectExamples = [
     (compSent (Sent Everyone Laughs) W1, False),
     (compSent (Sent Someone Laughed) W4, True),
     (compSent (Sent Someone Laughed) W3, True),
     (compSent (Sent Someone Laughed) W1, False),
     (compSent (Sent Someone Shudders) W4, False),
     (compSent (Sent Someone Shudders) W3, True),
     (compSent (Sent Someone Shudders) W2, False),
     (compSent (Sent Someone Shudders) W1, True),
     (compSent (Sent Someone (VP5 Will Shudder)) W1, True),
     (compSent (Sent Someone (VP5 Will Shudder)) W3, False),
     (compSent (Sent Someone (VP5 Will (INF Admire Dorothy))) W1, True),
     (compSent (Sent Someone (VP5 Would Shudder)) W2, True),
     (compSent (Sent Someone (VP5 Would (INF Admire Dorothy))) W1, False),
     (compSent (Sent Someone (VP5 Would (INF Admire Dorothy))) W2, True),
     (compSent (Sent Someone (VP6 Has LaughedEN)) W4, True),
     (compSent (Sent Someone (VP6 Has LaughedEN)) W3, True),
     (compSent (Sent Someone (VP6 Has LaughedEN)) W2, True),
     (compSent (Sent Someone (VP6 Has LaughedEN)) W1, False),
     (compSent (Sent Someone (VP6 Had LaughedEN)) W4, True),
     (compSent (Sent Someone (VP6 Had LaughedEN)) W3, True),
     (compSent (Sent Someone (VP6 Had LaughedEN)) W2, False),
     (compSent (Sent Someone (VP6 Had LaughedEN)) W1, False)
    ]

anaphorExamples = [
    (compText [(Sent Alice Laughed), (Sent She Cheered)] W2, True),
    (compSent  (Sent (NP1 Some Girl) Laughed) W2, True),
    (compSent  (Sent (NP1 Some Girl) Shuddered) W2, True),
    (compText [(Sent (NP1 Some Girl) Laughed), (Sent She Shuddered)] W2, False)
    ]

determinerExamples = [
    (compSent (Sent (NP1 Some Princess) Laughs) W2, True),
    (compSent (Sent (NP1 Some Dwarf) Laughs) W2, True),
    (compSent (Sent Atreyu (VP1 Loved (NP1 Some Sword))) W2, True),
    (compSent (Sent Atreyu (VP1 Loved (NP1 His Sword))) W2, True),
    (compSent (Sent LittleMook (VP1 Loved (NP1 His Sword))) W2, True)
    ]

determinerAnaphorExamples = [
    (compText [(Sent (NP1 Some Wizard) Shudders), (Sent He Laughs)] W3, True),
    (compText [(Sent Atreyu (VP1 Defeated (NP1 Some Wizard))), (Sent He Laughed)] W2, True),
    (compText [(Sent Atreyu (VP1 Defeated (NP1 Some Wizard))), (Sent He (VP1 Loved (NP1 His Sword)))] W2, False), -- because he refers to the wizard, not Atreyu
    (compText [(Sent Atreyu (VP1 Loved (NP1 His Sword))), (Sent It (VP1 Defeated (NP1 Some Wizard)))] W2, True),
    (compText [(Sent Atreyu (VP1 Loved (NP1 His Sword))), (Sent It (VP1 Defeated (NP1 Some Wizard))), (Sent He Laughed)] W2, True)
    ]

ellipsisExamples = [
    (compText [(Sent Alice Cheered), (Sent LittleMook (VP4 Did Too))] W2, True),
    (compText [(Sent Alice Laughed), (Sent LittleMook (VP4 Did Too))] W2, False),
    (compText [(Sent Goldilocks (VP1 Admired Dorothy)), (Sent Atreyu (VP4 Did Too))] W2, True),
    (compText [(Sent Goldilocks (VP1 Admired Dorothy)), (Sent LittleMook (VP4 Did Too))] W2, False),
    (compText [(Sent Alice Cheers), (Sent LittleMook (VP4 Does Too))] W1, True),
    (compText [(Sent Alice Laughs), (Sent LittleMook (VP4 Does Too))] W1, False),
    (compText [(Sent Alice Cheers), (Sent Dorothy (VP4 Did Too))] W2, True),
    (compText [(Sent Dorothy Cheered), (Sent Alice (VP4 Does Too))] W2, True),
    (compText [(Sent Dorothy Cheers), (Sent Alice (VP4 Will Too))] W1, True),
    (compText [(Sent LittleMook (VP5 Will Cheer)), (Sent Alice (VP4 Will Too))] W1, True),
    (compText [(Sent Dorothy Cheered), (Sent Alice (VP4 Will Too))] W2, True),
    (compText [(Sent Atreyu Shuddered), (Sent (NP1 Some Wizard) (VP4 Would Too))] W2, True),
    (compText [(Sent (NP1 Some Princess) Laughed), (Sent Goldilocks (VP7 Had Too))] W3, True),
    (compText [(Sent (NP1 Some Princess) (VP6 Had LaughedEN)), (Sent Goldilocks (VP7 Had Too))] W3, True),
    (compText [(Sent Goldilocks (VP1 Admires Dorothy)), (Sent Atreyu (VP4 Will Too))] W1, True),
    (compText [(Sent Goldilocks (VP1 Admired (NP1 Some Girl))), (Sent Atreyu (VP4 Did Too))] W2, True),
    (compText [(Sent (NP1 Some Princess) Laughs), (Sent (NP1 Some Dwarf) (VP4 Does Too))] W2, True)
    ]

unsaturatedEllipsisExamples = [
    (compText [(Sent Atreyu (VP1 Loved (NP1 His Sword))), (Sent LittleMook (VP4 Did Too))] W2, True),
    (compText [(Sent Atreyu (VP1 Loved (NP1 His Sword))), (Sent SnowWhite (VP4 Did Too))] W2, False),
    (compText [(Sent Atreyu (VP1 Loves (NP1 His Sword))), (Sent LittleMook (VP4 Does Too))] W2, False),
    (compText [(Sent Atreyu (VP1 Loves (NP1 His Sword))), (Sent LittleMook (VP4 Did Too))] W2, False),
    (compText [(Sent Atreyu (VP1 Loved (NP1 His Sword))), (Sent Alice (VP4 Did Too))] W2, False),
    (compText [(Sent Alice (VP1 Loved (NP1 Her Sword))), (Sent LittleMook (VP4 Did Too))] W2, False)
    ]

badExamples = [
    -- doesn't work: our sel function grabs Atreyu instead of Dorothy
    (compText [(Sent Dorothy Laughed), (Sent Atreyu (VP1 Admired She))] W2, True)
    ]

testExamples :: [(Bool, Bool)] -> Bool
testExamples examples = all (\tup -> fst tup == snd tup) examples

isCorrect = all testExamples [examples, tenseAspectExamples, anaphorExamples, determinerExamples,
    determinerAnaphorExamples, ellipsisExamples, unsaturatedEllipsisExamples]

isBroken :: [(Bool, Bool)] -> Bool
isBroken examples = any (\tup -> fst tup /= snd tup) examples