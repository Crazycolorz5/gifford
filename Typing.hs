module Typing where

import Control.Monad
import Lang

type Environment = Id -> Maybe Type

update e x y = \x' -> if x == x' then Just y else e x

emptyEnv = const Nothing

typeEffectOf :: Environment -> Expression -> Maybe (Type, Effect)
typeEffectOf _ (Literal _) = Just (TInt, pureEf)
typeEffectOf env (Var x) = do
    t <- env x
    return (t, pureEf)
typeEffectOf env (Lambda x t e) = do
    let env' = update env x t
    (t', ef) <- typeEffectOf env' e
    return (TFunc t ef t', pureEf)
typeEffectOf env (App e1 e2) = do
    (temp, ef1) <- typeEffectOf env e1
    case temp of
        TFunc t1 ef t2 -> do
            (t, ef2) <- typeEffectOf env e2
            guard (subtype t t1)
            return (t2, maxeff [ef1, ef2, ef])
        _ -> mzero
typeEffectOf env (New r t e) = do
    (t', ef) <- typeEffectOf env e
    guard (subtype t' t)
    return (TRef r t, maxeff [ef, Alloc r])
typeEffectOf env (Get e) = do
    (temp, ef) <- typeEffectOf env e
    case temp of
         TRef r t -> return (t, maxeff [ef, Read r])
         _ -> mzero
typeEffectOf env (Set e1 e2) = do
    (temp, ef1) <- typeEffectOf env e1
    case temp of
         TRef r t -> do
             (t', ef2) <- typeEffectOf env e2
             guard (subtype t' t)
             return (TUnit, maxeff [ef1, ef2, Write r])
         _ -> mzero
    


typeOf = curry (fmap fst . uncurry typeEffectOf)
effectOf = curry (fmap snd . uncurry typeEffectOf)

{-
    | Var Id
    | Lambda Id Type Expression
    | App Expression Expression
    | New Region Type Expression
    | Get Expression
    | Set Expression Expression
-}
