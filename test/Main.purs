module Test.Main where

import Prelude                      (class Eq, class Semigroup, class Show, Unit, discard, negate, unit, ($), (<), (<<<))
import Data.Either                  (Either(..))
import Data.Form.Decoder          as D
import Data.Generic.Rep             (class Generic)
import Data.Generic.Rep.Show        (genericShow)
import Data.Maybe                   (Maybe(..))
import Data.Validation.Semigroup  as V
import Effect                       (Effect)
import Test.Spec                    (describe, it)
import Test.Spec.Assertions         (shouldEqual)
import Test.Spec.Reporter.Console   (consoleReporter)
import Test.Spec.Runner             (run)

data TestType
  = TypeA
  | TypeB

derive instance eqTestType :: Eq TestType
derive instance genericTestType :: Generic TestType _
instance showTestType :: Show TestType where
  show = genericShow

main :: Effect Unit
main =
  run [consoleReporter] do
    describe "Data.Form.Decoder" do
      describe "Decode functions" do
        it "run" do
          (V.toEither $ D.run (D.int "Invalid") "foo") `shouldEqual` (Left "Invalid")
          (V.toEither $ D.run (D.int "Invalid") "34")  `shouldEqual` (Right 34)

        it "identity" do
          (runIdentity "foo") `shouldEqual` (Right "foo")
          (runIdentity 34)    `shouldEqual` (Right 34)

        it "always" do
          (runAlways "bar" "foo") `shouldEqual` (Right "bar")
          (runAlways 34 23)       `shouldEqual` (Right 34)

        it "fail" do
          (runFail ["error"] "foo")     `shouldEqual` (Left ["error"])
          (runFail ["error"] (Just 34)) `shouldEqual` (Left ["error"])

        it "int" do
          (V.toEither $ D.run (D.int "Invalid") "foo")  `shouldEqual` (Left "Invalid")
          (V.toEither $ D.run (D.int "Invalid") "34.3") `shouldEqual` (Left "Invalid")
          (V.toEither $ D.run (D.int "Invalid") "34e3") `shouldEqual` (Left "Invalid")
          (V.toEither $ D.run (D.int "Invalid") "34")   `shouldEqual` (Right 34)

        it "number" do
          (V.toEither $ D.run (D.number "Invalid") "foo")  `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.number "Invalid") "34.3") `shouldEqual` (Right (34.3    :: Number))
          (V.toEither $ D.run (D.number "Invalid") "34e3") `shouldEqual` (Right (34000.0 :: Number))
          (V.toEither $ D.run (D.number "Invalid") "34")   `shouldEqual` (Right (34.0    :: Number))

        it "symbol" do
          (V.toEither $ D.run (D.symbol "Invalid" TypeA "TypeA") "foo"  ) `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.symbol "Invalid" TypeB "TypeB") "34.3" ) `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.symbol "Invalid" TypeA "TypeA") "TypeA") `shouldEqual` (Right TypeA)
          (V.toEither $ D.run (D.symbol "Invalid" TypeB "TypeB") "TypeB") `shouldEqual` (Right TypeB)

      describe "Primitive validators" do
        it "validater" do
          (V.toEither $ (D.run (D.validator "Invalid" \_ -> false) "foo")) `shouldEqual` (Left  "Invalid")
          (V.toEither $ (D.run (D.validator "Invalid" \_ -> true)  "foo")) `shouldEqual` (Right unit)

        it "required" do
          (V.toEither $ D.run (D.required "Invalid") "")    `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.required "Invalid") "foo") `shouldEqual` (Right unit)

        it "minBound" do
          (V.toEither $ D.run (D.minBound "Invalid" 0) (-1)) `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.minBound "Invalid" 0) 0)    `shouldEqual` (Right unit)

        it "maxBound" do
          (V.toEither $ D.run (D.maxBound "Invalid" 0) 1) `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.maxBound "Invalid" 0) 0) `shouldEqual` (Right unit)

        it "minLength" do
          (V.toEither $ D.run (D.minLength "Invalid" 6) "short")  `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.minLength "Invalid" 6) "looong") `shouldEqual` (Right unit)

        it "maxLength" do
          (V.toEither $ D.run (D.maxLength "Invalid" 5) "looong") `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.maxLength "Invalid" 5) "short")  `shouldEqual` (Right unit)

      describe "Helper functions to validation" do
        it "assert" do
          (V.toEither $ D.run (D.assert (D.required "Required") (D.int "Invalid")) "")    `shouldEqual` (Left  "Required")
          (V.toEither $ D.run (D.assert (D.required "Required") (D.int "Invalid")) "foo") `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.assert (D.required "Required") (D.int "Invalid")) "34")  `shouldEqual` (Right 34)

        it "when" do
          (runFailWhen (_ < 0) ["error"] (-1)) `shouldEqual` (Left ["error"])
          (runFailWhen (_ < 0) ["error"] 0)    `shouldEqual` (Right unit)

        it "unless" do
          (runFailUnless (_ < 0) ["error"] 0)    `shouldEqual` (Left ["error"])
          (runFailUnless (_ < 0) ["error"] (-1)) `shouldEqual` (Right unit)

      describe "Function to build up decoder for forms" do
        it "hoist" do
          (V.toEither $ D.run (D.hoist _.f1 $ D.int "Invalid") { f1: "foo", f2: "34" }) `shouldEqual` (Left  "Invalid")
          (V.toEither $ D.run (D.hoist _.f2 $ D.int "Invalid") { f1: "foo", f2: "34" }) `shouldEqual` (Right 34)

    where
      runIdentity :: forall a. a -> Either String a
      runIdentity = V.toEither <<< (D.run D.identity)
      
      runAlways :: forall a b. a -> b -> Either String a
      runAlways o = V.toEither <<< D.run (D.always o)

      runFail :: forall a b. a -> b -> Either a b
      runFail o = V.toEither <<< D.run (D.fail o)

      runFailWhen :: forall e i. Semigroup e => (i -> Boolean) -> e -> i -> Either e Unit
      runFailWhen f o = V.toEither <<< D.run (D.when f $ D.fail o)

      runFailUnless :: forall e i. Semigroup e => (i -> Boolean) -> e -> i -> Either e Unit
      runFailUnless f o = V.toEither <<< D.run (D.unless f $ D.fail o)
