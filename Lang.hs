module Lang where

type Id = String

type Loc = (Region, Type)

data Expression =
      Literal Int
    | Location Loc
    | Var Id
    | Lambda Id Type Expression
    | App Expression Expression
    | New Region Type Expression
    | Get Expression
    | Set Expression Expression

data Value =
      VLiteral Int
    | VVar Id
    | VLambda Id Type Expression

expOfVal (VLiteral x) = Literal x
expOfVal (VVar x) = Var x
expOfVal (VLambda x t e) = Lambda x t e

valOfExp :: Expression -> Value
valOfExp (Literal x) = VLiteral x
valOfExp (Var x) = VVar x
valOfExp (Lambda x t e) = VLambda x t e
valOfExp x = error "Expression is not a value."

isValue e = case e of
    Literal _ -> True
    Var _ -> True
    Lambda _ _ _ -> True
    _ -> False

data Type =
      TUnit
    | TInt
    | TFunc Type Effect Type
    | TRef Region Type

data Effect =
      EVar Id
    | Alloc Region
    | Read Region
    | Write Region
    | Maxeff [Effect]
-- Pure is Maxeff []
pureEf = Maxeff []
-- Constructor for making Maxeff a list and not a tree
maxeff_bin (Maxeff e1s) (Maxeff e2s) = Maxeff (e1s ++ e2s)
maxeff_bin (Maxeff e1s) e2 = Maxeff (e2 : e1s)
maxeff_bin e1 (Maxeff e2s) = Maxeff (e1 : e2s)
maxeff_bin e1 e2 = Maxeff [e1, e2]

maxeff :: [Effect] -> Effect
maxeff l = foldl (\acc e -> maxeff_bin e acc) (Maxeff []) l

data Region = 
      RConst
    | RVar Id
    | Union Region [Region]

subtype TUnit TUnit = True
subtype TUnit _ = False
subtype TInt TInt = True
subtype TInt _ = False
subtype (TFunc t1 ef t2) (TFunc t1' ef' t2') = subtype t1 t1' && subeffect ef ef' && subtype t2 t2'
subtype (TFunc _ _ _) _ = False
subtype (TRef r t) (TRef r' t') = subregion r r' && subtype t t'

subeffect _ _ = True -- TODO

subregion (Union r rs) r2@(Union r' r's) = subregion r r2 && all (flip subregion r2) rs
subregion r (Union r' rs) = subregion r r' || any (subregion r) rs
subregion RConst RConst = True
subregion RConst _ = False
subregion (RVar x) (RVar y) = x == y
subregion (RVar _) _ = False
