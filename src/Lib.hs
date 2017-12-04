module Lib
    ( normalize
    , extractOne
    , reduce
    , checkFinal
    , check
    , combine
    ) where

import Types
import Parser

import qualified Data.Map as Map
import Data.Map((!))
import Data.Ratio

fromRight (Right x) = x

check :: EquationSet -> Bool
check es = if numberOfVarsLeft es > 1 then check (reduce es) else checkFinal es

numberOfVarsLeft [] = 0
numberOfVarsLeft es = maximum . map (length . Map.keys . leftSide) $ es

-- |Check if the inequality with one variable is satisfiable
-- this function is similar to the reduce in the first half
checkFinal :: EquationSet -> Bool
checkFinal es = boundsOk upperBound lowerBound && noVarsOk
    where
        boundsOk (Just u) (Just l) = u >= l
        boundsOk _ _ = True

        noVarsOk :: Bool
        noVarsOk =  all (== True) . (map checkNoVar) $ esWithNoVars

        upperBound, lowerBound :: Maybe (Ratio Int)
        upperBound = if length constantsA > 0 then Just (maximum constantsA) else Nothing
        lowerBound = if length constantsB > 0 then Just (minimum constantsB) else Nothing

        constantsA = map constant $ setA
        constantsB = map constant $ setB

        setA = filter isA esWith1VarNormalized
        setB = filter isB esWith1VarNormalized

        variable = head . Map.keys . leftSide . head $ es
        esWith1VarNormalized = map (extractOne variable) esWith1Var
        esWith1Var = filter (hasNVars 1) es
        esWithNoVars = filter (hasNVars 0) es

        hasNVars n e = (length . Map.keys . leftSide $ e) == n

        -- checks if an inequality with no variables is OK
        checkNoVar e = if isA e then constant e > 0 else constant e < 0

isA e = inequalityType e == GreaterThan || inequalityType e == GreaterEqual
isB e = inequalityType e == LessThan || inequalityType e == LessEqual

reduce :: EquationSet -> EquationSet
reduce equations = [ combine a b | a <- setA, b <- setB ] ++ setC
    where        
        setA = filter isA equations'
        setB = filter isB equations'
        setC = filter (not . hasThatVar) equations

        equations' = map (extractOne variable) . filter hasThatVar $ equations
        hasThatVar e = variable `Map.member` leftSide e

        variable = head . Map.keys . leftSide . head $ equations

combine :: ReducibleInequality -> ReducibleInequality -> ReducibleInequality
combine a b = ReducibleInequality vars' constant' inequalityType'
    where
        inequalityType' = inequalityType a
        vars' = (leftSide a) `combineVars` (leftSide b)
        constant' = constant a - constant b

        combineVars va vb = Map.unionsWith (+) [va, Map.map (* (-1)) vb]

extractOne :: Char -> ReducibleInequality -> ReducibleInequality
extractOne var e = if var `Map.member` leftSide e then ReducibleInequality vars' constant' inequalityType'
                                                  else e
    where
        vars' = Map.filterWithKey (\k _ -> k /= var) . Map.map (normalizeCoeff signOfVar) $ leftSide e
        constant' = normalizeCoeff signOfVar . constant $ e
        inequalityType' = flipOpIfSign signOfVar $ inequalityType e

        flipOpIfSign True LessThan = GreaterThan
        flipOpIfSign True GreaterThan = LessThan
        flipOpIfSign True LessEqual = GreaterEqual
        flipOpIfSign True GreaterEqual = LessEqual
        flipOpIfSign _ Equation = Equation
        flipOpIfSign False op = op

        normalizeCoeff sign x = (-1 * x) / coeff

        signOfVar = if coeff >= 0 then True else False
        coeff = (leftSide e) ! var

normalize :: RawInequality -> ReducibleInequality
normalize (RawInequality leftSide rightSide op) = ReducibleInequality vars (constant % 1) inequalityType
    where
        vars = Map.fromList . map varTokenToPair $ (leftVarTokens ++ map flipToken rightVarTokens)

        varTokenToPair (VarToken i c) = (c, i % 1)
        varTokenToPair _ = error "Can only be called on var tokens"

        flipToken (VarToken i c) = VarToken (-i) c
        flipToken _ = error "Can only be called on var tokens"

        rightVarTokens = filter isAVarToken rightSide
        leftVarTokens = filter isAVarToken leftSide
        isAVarToken (VarToken _ _) = True
        isAVarToken _ = False

        constant = sum leftSideConstants - sum rightSideConstants
        leftSideConstants = extractConstantValues $ leftSide
        rightSideConstants = extractConstantValues $ rightSide

        extractConstantValues = map getConstant . filter isAConstantToken

        getConstant (ConstantToken x) = x
        getConstant _ = error "Can only be called on constant tokens"

        isAConstantToken (ConstantToken _) = True
        isAConstantToken _ = False

        inequalityType = opStringToInequalityType op

        opStringToInequalityType ">" = GreaterThan
        opStringToInequalityType "<" = LessThan
        opStringToInequalityType "<=" = LessEqual
        opStringToInequalityType ">=" = GreaterEqual
        opStringToInequalityType "=" = Equation
        opStringToInequalityType _  = error "Invalid operator type for the inequality"

