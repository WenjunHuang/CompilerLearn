module LL1.Parser where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except)
import Control.Monad.State (State)
import Control.Monad.State.Class (get)
import Data.Either (Either(..))
import Data.Foldable (all, foldl, foldr)
import Data.List (List, (:))
import Data.List.Types (List(..))
import Data.Map (Map)
import Data.Set (Set, empty,singleton)
import Data.String (Pattern(..))
import Data.String.Common (split)
import Data.List as L
import Data.Set as S

type Token = String

type Production =
 { lhs :: Token
   , rhs :: List Token
 }

type Location =
    { line :: Int,
      column :: Int}

type LocationRange =
    { start :: Location,
      end :: Location}
type ParserError =
    { location :: LocationRange,
      message :: String}

type LL1Table =
    { grammar :: Grammar,
    nullable :: Set Token,
    first :: Map String (Set Token),
    follow :: Map String (Set Token),
    transition :: Map String (Map String (List Production))
    }

type Grammar =
    { rules :: List Production,
      terminals :: Set Token,
      nonTerminals :: Set Token,
      start :: String}

type Parser =
    { ll1Table :: LL1Table,
     tokenStream :: List Token,
     position :: Int,
     rule :: String,
     stack :: Array Token}

synthetic_start :: String
synthetic_start  = "S"

sentinel :: String
sentinel = "$"

epsilon :: String
epsilon = "ε"

parseProduction :: LocationRange -> String -> Either ParserError Production
parseProduction loc input =
    case split (Pattern "->") input of
        [lhs,rhs] ->
          let tokens = L.fromFoldable (split (Pattern " ") rhs)
          in Right { lhs : lhs,
              rhs : tokens}
        _ -> Left {location: loc, message: "Invalid production format" }

defaultLocationRange :: LocationRange
defaultLocationRange = { start: { line: 1, column: 1 }, end: { line: 1, column: 1 } }

partitionEithers :: forall a b . List (Either a b) -> { lefts :: List a, rights :: List b }
partitionEithers eithers = foldr partitioner {lefts: Nil, rights: Nil} eithers
    where
        partitioner :: Either a b -> {lefts :: List a, rights :: List b} -> {lefts :: List a, rights :: List b}
        partitioner either acc =
            case either of
                Left a -> {lefts: a : acc.lefts, rights: acc.rights}
                Right b -> {lefts: acc.lefts, rights: b :  acc.rights}

extractProductions :: List Production -> {nonTerminals :: Set Token, terminals :: Set Token}
extractProductions productions =
    let
        nonTerminals = foldr (\production acc -> S.insert production.lhs acc) S.empty productions
        terminals = foldr (\production acc -> acc <> (foldr (\token acc ->
            if S.member token nonTerminals then acc
            else S.insert token acc) S.empty production.rhs)) S.empty productions
    in
        {nonTerminals: nonTerminals, terminals: terminals}

lexGrammar ::  ExceptT (List ParserError) (State String) Grammar
lexGrammar = do
    input <- get
    let
        lines = L.fromFoldable $ split (Pattern "\n") input
        parsedProductions = map (parseProduction defaultLocationRange) lines
        partitioned = partitionEithers parsedProductions
    case partitioned.lefts of
            Nil ->
                case partitioned.rights of
                    Nil -> except $ Left ({location: defaultLocationRange, message: "No productions found"}:Nil)
                    productions@(x:_) ->
                        let
                            {terminals,nonTerminals} = extractProductions productions
                        in
                            pure {rules: partitioned.rights, terminals, nonTerminals, start: x.lhs}
            errors -> do
                except $ Left errors

computeNullable :: Grammar -> Set Token
computeNullable grammar =
 let
    initialNullable = S.singleton epsilon
    step :: Set Token -> Set Token
    step acc =
        foldl (\acc' rule ->
            if all (\x -> S.member x acc') rule.rhs && not (S.member rule.lhs acc')
            then S.insert rule.lhs acc'
            else acc') acc grammar.rules
    fixedPoint :: Set Token -> Set Token
    fixedPoint acc =
        let next = step acc
        in if next == acc then acc else fixedPoint next
 in
    fixedPoint initialNullable

computeFirst :: Grammar -> Set Token -> Map String (Set Token)
computeFirst grammar nullable =

parseGrammar :: String -> Either (List ParserError) Grammar
parseGrammar input =
    let
        lines = L.fromFoldable $ split (Pattern "\n") input
        parsedProductions = map (parseProduction defaultLocationRange) lines
        partitioned = partitionEithers parsedProductions
    in
        case partitioned.lefts of
            Nil ->
                case partitioned.rights of
                    Nil -> Left ({location: defaultLocationRange, message: "No productions found"}:Nil)
                    productions@(x:_) ->
                        let
                            {terminals,nonTerminals} = extractProductions productions
                        in
                            Right {rules: partitioned.rights, terminals, nonTerminals, start: x.lhs}
            errors -> Left errors
