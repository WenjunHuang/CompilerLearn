module Eval where

typeCheck :: [String] -> Either String ()
typeCheck 
  | op:xs if length xs == 2 = 
    case op of
      "+" -> 
        if (head xs) == "TInt" && (last xs) == "TInt"
          then Right ()
          else Left "Type error"
      _ -> Left "Type error"