module Grammar where

import Data.List

data Sent = Sent NP VP deriving Show
data NP   = SnowWhite  | Alice  | Dorothy | Goldilocks 
          | LittleMook | Atreyu | Everyone | Someone
          -- Use `He` and `She` for `Him` and `Her` (ignore morphology, also so that `Her` is free for possessive below)
          | He | She | It
          | NP1 DET CN
          deriving Show
data DET  = Every | Some
          | His | Her | Its
          deriving Show
data CN   = Girl   | Boy   | Princess | Dwarf | Giant 
          | Wizard | Sword | Dagger
          deriving Show
data VP   = Laughed | Laughs | Cheered | Cheers | Shuddered | Shudders
          | VP1 TV NP
          | VP4 AUX Too
          | VP5 AUX INF
          | VP6 PastAUX EN
          | VP7 PastAUX Too
          deriving Show
data TV   = Loved | Loves | Admired | Admires | Helped | Helps
          | Defeated | Defeats
          deriving Show
data AUX   = Did | Does | Will | Would
            deriving Show
data PastAUX = Has | Had deriving Show
data INF  = Laugh | Cheer | Shudder | INF TINF NP deriving Show
data TINF = Love | Admire | Help | Defeat
            deriving Show
-- Past participles, -EN form (had laughed, ...); because for all verbs in this example the EN and ED forms are spelled
-- the same, these ones need different names
data EN = LaughedEN | CheeredEN | ShudderedEN | EN TEN NP deriving Show
data TEN = LovedEN | AdmiredEN | HelpedEN | DefeatedEN deriving Show
data Too  = Too deriving Show

