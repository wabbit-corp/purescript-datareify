module Data.Reified (Graph(..), toGraph, fromGraph) where

import Prelude
import Effect (Effect)

import Control.Monad.State.Trans (StateT, get, modify, runStateT)
import Effect.Class (liftEffect)

import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Traversable (class Traversable, traverse)
import Data.HashMap as HM

import Effect.StableName (StableName, makeStableName)

import Matryoshka.Class.Recursive (class Recursive, project)
import Matryoshka.Class.Corecursive (class Corecursive, embed)

newtype Graph f = Graph (HM.HashMap Int (f Int))

type State t f = {
  size  :: Int,
  names :: HM.HashMap (StableName t) Int,
  graph :: HM.HashMap Int (f Int)
}

emptyState :: forall t f. State t f
emptyState = { size: 0, names: HM.empty, graph: HM.empty }

toGraph :: forall t f. Recursive t f => Traversable f => t -> Effect (Graph f)
toGraph node = Graph <$> (\x -> x.graph) <$> snd <$> runStateT (toGraph' node) emptyState

toGraph' :: forall t f. Recursive t f => Traversable f => t -> StateT (State t f) Effect Int
toGraph' node = do
  name <- liftEffect $ makeStableName node
  oldState <- get

  case HM.lookup name oldState.names of
    Just key -> pure key
    Nothing  -> do
      _ <- modify $ \s -> s {
        size = oldState.size + 1,
        names = HM.insert name oldState.size oldState.names
      }

      entry <- traverse toGraph' $ project node
      _ <- modify $ \s -> s { graph = HM.insert oldState.size entry s.graph }
      pure oldState.size

fromGraph :: forall t f. Partial => Corecursive t f => Traversable f => Graph f -> t
fromGraph g = fromGraph' g 0

fromGraph' :: forall t f. Partial => Corecursive t f => Traversable f => Graph f -> Int -> t
fromGraph' (Graph m) i = case HM.lookup i m of
  Just fa -> embed (fromGraph' (Graph m) <$> fa)