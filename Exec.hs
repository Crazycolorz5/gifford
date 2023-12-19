module Exec where

import Lang

type Store = Loc -> Value

type State = (Expression, Store)


reduce :: State -> Maybe State
reduce = uncurry reduce'

reduce' (App (Lambda x t e) v) sigma | isValue v = return (subst e v x, sigma)
reduce' (New r t v) sigma | notBound l sigma = 

subst :: Expression -> Expression -> Id -> Expression
subst e v x = e --TODO
