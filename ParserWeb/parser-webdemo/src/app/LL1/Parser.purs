module LL1.Parser where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except)
import Control.Monad.State (State)
import Control.Monad.State.Class (get)
import Data.Either (Either(..))
import Data.Foldable (all, foldMap, foldl, foldr)
import Data.Functor (map)
import Data.List (List, filter, mapMaybe, reverse, takeWhile, (:))
import Data.List.Types (List(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set, empty, singleton)
import Data.String (Pattern(..))
import Data.String.Common (split, trim)
import Data.Array as A
import Data.List as L
import Data.Map as M
import Data.Set as S

type Token = String

type Production =
  { lhs :: Token
  , rhs :: List Token
  }

type Location =
  { line :: Int
  , column :: Int
  }

type LocationRange =
  { start :: Location
  , end :: Location
  }

type ParserError =
  { location :: LocationRange
  , message :: String
  }

type LL1Table =
  { grammar :: Grammar
  , nullable :: Set Token
  , first :: Map String (Set Token)
  , follow :: Map String (Set Token)
  , transition :: Map String (Map String (List Production))
  }

type Grammar =
  { rules :: List Production
  , terminals :: Set Token
  , nonTerminals :: Set Token
  , start :: String
  }

grammarTerminals :: Grammar -> Array String
grammarTerminals grammar = A.fromFoldable grammar.terminals

grammarNonTerminals :: Grammar -> Array String
grammarNonTerminals grammar = A.fromFoldable grammar.nonTerminals

grammarStart :: Grammar -> String
grammarStart = _.start

type Parser =
  { ll1Table :: LL1Table
  , tokenStream :: List Token
  , position :: Int
  , rule :: String
  , stack :: Array Token
  }

data AstNode = TerminalNode { token :: Token } | NonTerminalNode { token :: Token, children :: List AstNode }

synthetic_start :: String
synthetic_start = "S"

sentinel :: String
sentinel = "$"

epsilon :: String
epsilon = "ε"

parseProduction :: LocationRange -> String -> Either ParserError Production
parseProduction loc input =
  case split (Pattern "->") input of
    [ lhs, rhs ] ->
      let
        tokens = filter (\x -> x /= "") $ L.fromFoldable (split (Pattern " ") rhs)
      in
        Right
          { lhs: trim lhs
          , rhs: tokens
          }
    _ -> Left { location: loc, message: "Invalid production format" }

defaultLocationRange :: LocationRange
defaultLocationRange = { start: { line: 1, column: 1 }, end: { line: 1, column: 1 } }

partitionEithers :: forall a b. List (Either a b) -> { lefts :: List a, rights :: List b }
partitionEithers eithers = foldr partitioner { lefts: Nil, rights: Nil } eithers
  where
  partitioner :: Either a b -> { lefts :: List a, rights :: List b } -> { lefts :: List a, rights :: List b }
  partitioner either acc =
    case either of
      Left a -> { lefts: a : acc.lefts, rights: acc.rights }
      Right b -> { lefts: acc.lefts, rights: b : acc.rights }

extractProductions :: List Production -> { nonTerminals :: Set Token, terminals :: Set Token }
extractProductions productions =
  let
    nonTerminals = foldr (\production acc -> S.insert production.lhs acc) S.empty productions
    terminals = foldr
      ( \production acc -> acc <>
          ( foldr
              ( \token acc ->
                  if S.member token nonTerminals then acc
                  else S.insert token acc
              )
              S.empty
              production.rhs
          )
      )
      S.empty
      productions
  in
    { nonTerminals: nonTerminals, terminals: terminals }

computeNullable :: Grammar -> Set Token
computeNullable grammar =
  fixedPoint initialNullable
  where
  initialNullable = S.singleton epsilon

  step :: Set Token -> Set Token
  step acc =
    foldl
      ( \acc' rule ->
          if all (\x -> S.member x acc') rule.rhs && not (S.member rule.lhs acc') then S.insert rule.lhs acc'
          else acc'
      )
      acc
      grammar.rules

  fixedPoint :: Set Token -> Set Token
  fixedPoint acc =
    let
      next = step acc
    in
      if next == acc then acc else fixedPoint next

reachableTerminals :: List String -> Map String (Set Token) -> Set String -> Set String
reachableTerminals rhs first nullable =
  let
    prefix = takeWhile (\x -> S.member x nullable) rhs
    t1 = mapMaybe (\sym -> M.lookup sym first) prefix
    r = foldl (\acc x -> acc <> x) S.empty t1
  in
    r

computeFirst :: Grammar -> Set Token -> Map String (Set Token)
computeFirst grammar nullable = fixedPoint initial
  where
  initial = foldMap (\t -> M.singleton t S.empty) grammar.nonTerminals
    <> foldMap (\t -> M.singleton t (S.singleton t)) grammar.terminals

  step :: Map String (Set Token) -> Map String (Set Token)
  step acc = foldl update acc grammar.rules
    where
    update acc' rule =
      let
        firstOfRhs = fromMaybe mempty $ M.lookup rule.lhs acc'
        r = reachableTerminals rule.rhs acc nullable
      in
        M.insert rule.lhs (firstOfRhs <> r) acc'

  fixedPoint :: Map String (Set Token) -> Map String (Set Token)
  fixedPoint acc = if step acc == acc then acc else fixedPoint (step acc)

computeFollow :: Grammar -> Set String -> Map String (Set Token) -> Map String (Set Token)
computeFollow grammar nullable first = fixedPoint $ foldMap (\nt -> M.singleton nt S.empty) grammar.nonTerminals
  where
  step :: Map String (Set Token) -> Map String (Set Token)
  step acc = foldl update acc grammar.rules
    where
    updateFollow { follow, temp } sym =
      if S.member sym grammar.terminals then
        { follow, temp: S.singleton sym }
      else if S.member sym grammar.nonTerminals then
        let
          temp' = maybe S.empty (\x -> x <> temp) (M.lookup sym follow)
          newTemp =
            if S.member sym nullable then
              maybe temp' (\x -> x <> temp') (M.lookup sym first)
            else fromMaybe S.empty (M.lookup sym first)
        in
          { follow: M.update (\old -> Just (old <> temp)) sym follow, temp: newTemp }
      else { follow, temp }
    update acc' rule =
      let
        temp = maybe S.empty identity (M.lookup rule.lhs acc')
        { follow } = foldl updateFollow { follow: acc', temp } (reverse rule.rhs)
      in
        follow

  fixedPoint acc = if step acc == acc then acc else fixedPoint (step acc)

computeTransition :: Grammar -> Set Token -> Map String (Set Token) -> Map String (Set Token) -> Map String (Map String (List Production))
computeTransition grammar nullable first follow =
  let
    ts = foldMap (\t -> M.singleton t Nil) grammar.nonTerminals
    trans = foldMap (\nt -> M.singleton nt ts) grammar.nonTerminals
    f = foldl
      ( \trans' rule ->
          let
            b = if all (\x -> S.member x nullable) rule.rhs then fromMaybe S.empty (M.lookup rule.lhs follow) else S.empty
            update trans'' t =
              M.update
                ( \v ->
                    case M.lookup t v of
                      Nothing -> Nothing
                      Just x -> Just (M.insert t (rule : x) v)
                )
                rule.lhs
                trans''
            tran1 = foldl update trans (b <> reachableTerminals rule.rhs first nullable)
          in
            tran1
      )
      trans
      grammar.rules
  in
    f

lexer :: ExceptT (List ParserError) (State String) Grammar
lexer = do
  input <- get
  let
    lines = L.fromFoldable $ split (Pattern "\n") input
    parsedProductions = map (parseProduction defaultLocationRange) lines
    partitioned = partitionEithers parsedProductions
  case partitioned.lefts of
    Nil ->
      case partitioned.rights of
        Nil -> except $ Left ({ location: defaultLocationRange, message: "No productions found" } : Nil)
        productions@(x : _) ->
          let
            { terminals, nonTerminals } = extractProductions productions
          in
            pure { rules: partitioned.rights, terminals, nonTerminals, start: x.lhs }
    errors -> do
      except $ Left errors

createLL1Table :: ExceptT (List ParserError) (State Grammar) LL1Table
createLL1Table = do
  except $ Left ({ location: defaultLocationRange, message: "No productions found" } : Nil)
--do
--  grammar <- get
--  let
--    nullable = computeNullable grammar
--    first = computeFirst grammar nullable
--    follow = computeFollow grammar nullable first
--    transition = computeTransition grammar nullable first follow
