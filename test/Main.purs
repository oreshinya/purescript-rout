module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Assert (assert, ASSERT)
import Rout (match, lit, int, param, end)
import Data.Maybe (fromMaybe)
import Control.Alt ((<|>))

data Route
  = Home
  | User Int
  | Users String
  | NotFound

derive instance routeEq :: Eq Route
route :: String -> Route
route url = fromMaybe NotFound $ match url $
  Home <$ end
  <|>
  Users <$> (lit "users" *> param "name") <* end
  <|>
  User <$> (lit "users" *> int) <* end

main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
  assert $ route "" == Home
  assert $ route "/" == Home
  assert $ route "/users?name=oreshinya" == Users "oreshinya"
  assert $ route "/users/42" == User 42
  assert $ route "/projects" == NotFound
