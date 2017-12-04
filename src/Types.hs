module Types where
import Data.Map
import Data.Ratio

-- Raw types for parsing
data Token = VarToken Int Char | ConstantToken Int deriving (Show, Eq)
type Side = [Token]
data RawInequality = RawInequality Side Side String deriving (Show, Eq)

data InequalityType = LessThan
                | GreaterThan
                | LessEqual
                | GreaterEqual
                | Equation
                deriving (Show, Eq)

data ReducibleInequality = ReducibleInequality {
  leftSide :: Map Char (Ratio Int),
  constant :: Ratio Int, -- assumed it's on the left side, and right side is 0
  inequalityType :: InequalityType
} deriving (Eq)

instance Show ReducibleInequality where
    show (ReducibleInequality l c i) = showl ++ showConstant c ++ " " ++ showType i ++ " 0"
        where
            showl = unwords . Prelude.map (\(k,v) -> showRatio v ++ [k] ++ " ") . Data.Map.toList $ l
            showType LessThan = "<"
            showType GreaterThan = ">"
            showType GreaterEqual = ">="
            showType LessEqual = "<="
            showType Equation = "="
            showConstant c = (if c >= 0 then "+" else "") ++ showRatio c
            showRatio r =
                show (numerator r) ++ if denominator r == 1
                  then ""
                  else "/" ++ show (denominator r)


type EquationSet = [ReducibleInequality]
