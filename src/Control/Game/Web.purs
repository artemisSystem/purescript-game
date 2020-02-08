module Control.Game.Web where

-- TODO: implement AnimGame using a custom `requestAnimationFrame` that runs in
-- Aff.

-- will need a function like this:
-- requestAnimationFrames :: Effect Unit -> Aff Void
-- how can i make it resolve from that? untilJust?
-- built from a function like this: Effect a -> Aff a (uses requestanimationframe)