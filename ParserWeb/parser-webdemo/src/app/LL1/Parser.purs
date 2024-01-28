module LL1.Parser where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Class (get)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, all, foldMap, foldl, foldr)
import Data.Functor (map)
import Data.List (List(..), mapMaybe, reverse, takeWhile, (:))
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable)
import Data.Set (Set, empty, singleton)
import Data.String (Pattern(..))
import Data.String.Common (joinWith, split, trim)
import Data.Traversable (class Traversable)
import Data.Array as A
import Data.Either as E
import Data.List as L
import Data.Map as M
import Data.Set as S
import Debug as Debug

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

showProduction :: Production -> String
showProduction production = production.lhs <> " -> " <> (joinWith " " (A.fromFoldable production.rhs))
firstTokensOf :: String -> LL1Table -> Array String
firstTokensOf nt ll1Table = A.fromFoldable $ fromMaybe S.empty (M.lookup nt ll1Table.first)

grammarTerminals :: LL1Table -> Array String
grammarTerminals ll1Table = A.fromFoldable ll1Table.grammar.terminals

grammarNonTerminals :: LL1Table -> Array String
grammarNonTerminals ll1Table = A.fromFoldable ll1Table.grammar.nonTerminals

grammarStart :: LL1Table -> String
grammarStart = _.grammar.start

hasNullable :: String -> LL1Table -> Boolean
hasNullable nt ll1Table = S.member nt ll1Table.nullable

firstOf :: String -> LL1Table -> Array String
firstOf nt ll1Table =
  A.fromFoldable $ fromMaybe S.empty (M.lookup nt ll1Table.first)

followOf :: String -> LL1Table -> Array String
followOf nt ll1Table =
  A.fromFoldable $ fromMaybe S.empty (M.lookup nt ll1Table.follow)

productionOf :: String -> String -> LL1Table -> Array Production
productionOf terminal nonTerminal ll1Table =
  maybe mempty A.fromFoldable $ M.lookup nonTerminal ll1Table.transition
    >>= M.lookup terminal

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
        tokens = L.filter (\x -> x /= "")  (L.fromFoldable $ split (Pattern " ") rhs)
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
  foldl updateTrans initTrans grammar.rules
  where
  ts = foldMap (\t -> M.singleton t Nil) grammar.nonTerminals
  initTrans = foldMap (\nt -> M.singleton nt ts) grammar.nonTerminals
  updateTrans trans rule =
    foldl (updateProduction rule) trans (followOfRuleNonTerm <> terms)
    where
    followOfRuleNonTerm = if all (\x -> S.member x nullable) rule.rhs then fromMaybe mempty (M.lookup rule.lhs follow) else mempty
    terms = reachableTerminals rule.rhs first nullable
    updateProduction rule productions term =
      M.update (\v -> Just $ M.update (\x -> Just (rule : x)) term v) rule.lhs productions

createLL1Table :: Grammar -> LL1Table
createLL1Table grammar =
  let
    nullable = Debug.spy "computeNullable" (computeNullable grammar)
    first = Debug.spy "computeFirst" (computeFirst grammar nullable)
    follow = computeFollow grammar nullable first
    transition = computeTransition grammar nullable first follow
  in
    { grammar, nullable, first, follow, transition }

parseLL1Table :: ExceptT (Array ParserError) (State String) LL1Table
parseLL1Table = do
  input <- get
  let
    lines = L.fromFoldable $ split (Pattern "\n") input
    parsedProductions = map (parseProduction defaultLocationRange) lines
    partitioned = partitionEithers parsedProductions
  case partitioned.lefts of
    Nil ->
      case partitioned.rights of
        Nil -> throwError [ { location: defaultLocationRange, message: "No productions found" } ]
        productions@(x : _) ->
          let
            { terminals, nonTerminals } = extractProductions productions
            grammar = { rules: partitioned.rights, terminals, nonTerminals, start: x.lhs }
          in
            pure $ createLL1Table grammar
    errors -> do
      throwError $ A.fromFoldable errors

createParser :: String -> Either (Array ParserError) LL1Table
createParser grammarSource = evalState (runExceptT parseLL1Table) grammarSource

type ParseContext =
  { ll1Table :: LL1Table
  , tokenStream :: Array Token
  , position :: Int
  , remainTokenStream :: Array Token
  , rule :: String
  , stack :: Array Token
  }

data ParseStepState
  = ParseCompleted ParseContext
  | ParseError ParseContext String
  | ParseStep ParseContext

startParse :: Array String -> LL1Table -> ParseStepState
startParse source ll1Table =
  ParseStep $
    { ll1Table
    , tokenStream: A.fromFoldable source
    , remainTokenStream: A.fromFoldable source
    , position: 0
    , rule: ""
    , stack: [ sentinel, grammarStart ll1Table ]
    }

nextStep :: ParseStepState -> Either String ParseStepState
nextStep (ParseCompleted _) = Left "Parse completed"
nextStep (ParseError _ _) = Left "Parse error"
nextStep (ParseStep context) = do
  currentToken <- E.note ("Token position:" <> (show context.position) <> " is not valid") (A.index context.tokenStream context.position)
  top <- E.note "Stack is empty" (A.last context.stack)
  let
    next =
      if S.member top context.ll1Table.grammar.terminals then
        if top == currentToken then ParseStep $ context
          { position = context.position + 1
          , remainTokenStream = A.drop 1 context.remainTokenStream
          , stack = A.dropEnd 1 context.stack
          }
        else ParseError context ("Unexpected token: " <> currentToken)
      else if S.member top context.ll1Table.grammar.nonTerminals then
        case (M.lookup top context.ll1Table.transition) >>= (\x -> M.lookup currentToken x) of
          Nothing -> ParseError context ("Unexpected token: " <> currentToken)
          Just Nil -> ParseError context ("Unexpected token: " <> currentToken)
          Just (x : _) -> ParseStep $ context { rule = x.lhs <> " -> " <> (joinWith " " (A.fromFoldable x.rhs)), stack = A.dropEnd 1 context.stack }
      else ParseCompleted context
  pure $ next

