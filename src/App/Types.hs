{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-- Needed for MonadWidget constraints used in aliases
{-# LANGUAGE TypeFamilies #-}

module App.Types (
    -- Types
    Route (..),
    AppState (..),
    AppEnv (..),
    AppEvents (..),
    CounterUpdate,
    -- Lenses
    appState_route,
    appState_counter,
    env_appState,
    ae_navigation,
    ae_counterUpdate,
    -- Type Aliases
    InsularViewWidget,
    InsularActionWidget,
    OpenViewWidget,
    OpenActionWidget,
) where

-- Import MonadWidget constraint definitions
-- Import MonadWidget constraint definitions
import Control.Lens (makeLenses, (^.))
import Data.Text (Text)
import Reflex
import Reflex.Dom
import Reflex.Dom.Class

-- Define the possible routes in our application
data Route = MyHome | About | Profile | Exercises | Garden deriving (Eq, Show, Read, Ord)

-- Define the type for our counter update function
type CounterUpdate = Int -> Int

-- --- Interface Records ---
data AppState t = AppState
    { _appState_route :: Dynamic t Route
    , _appState_counter :: Dynamic t Int
    }
makeLenses ''AppState

data AppEnv t = AppEnv -- Environment passed explicitly
    { _env_appState :: AppState t
    }
makeLenses ''AppEnv

data AppEvents t = AppEvents
    { _ae_navigation :: Event t Route
    , _ae_counterUpdate :: Event t CounterUpdate
    }
makeLenses ''AppEvents -- Generate _ae_navigation, _ae_counterUpdate

-- Semigroup/Monoid are essential for combining events from widgetHold easily
instance (Reflex t) => Semigroup (AppEvents t) where
    a <> b =
        AppEvents
            (leftmost [a ^. ae_navigation, b ^. ae_navigation])
            (leftmost [a ^. ae_counterUpdate, b ^. ae_counterUpdate])
instance (Reflex t) => Monoid (AppEvents t) where
    mempty = AppEvents never never

-- --- Role-Based Type Aliases (Using RankNTypes) ---
-- These aliases use the specific AppEnv / AppEvents defined above implicitly
-- via the constraints / return types in the functions that use them.
-- 1. Does *not* take AppEnv. Produces *local* output.
type InsularViewWidget t input output = forall m. (MonadWidget t m) => input -> m output

-- 2. Does *not* take AppEnv. *Produces* AppEvents.
type InsularActionWidget t input = forall m. (MonadWidget t m) => input -> m (AppEvents t)

-- 3. *Takes* AppEnv explicitly. Produces *local* output.
type OpenViewWidget t input output = forall m. (MonadWidget t m) => AppEnv t -> input -> m output

-- 4. *Takes* AppEnv explicitly. *Produces* AppEvents.
type OpenActionWidget t input = forall m. (MonadWidget t m) => AppEnv t -> input -> m (AppEvents t)
