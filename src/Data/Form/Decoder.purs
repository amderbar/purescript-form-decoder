module Data.Form.Decoder where

import Prelude                   hiding (when)
import Control.Alt                      (class Alt, (<|>))
import Control.Alternative              (class Alternative)
import Control.Plus                     (class Plus, empty)
import Data.Either               as      Either
import Data.Int                  as      Int
import Data.Number               as      Num
import Data.String               as      Str
import Data.Validation.Semigroup        (V(..))
import Data.Validation.Semigroup as      V

-- | Core type representing a decoder.
-- |  It decodes input into type `input`, raising error of type `err`.
newtype Decoder err input output
  = Decoder (input -> V err output)

instance functorDecoder :: Functor (Decoder e i) where
  map f (Decoder v) = Decoder $ (map f) <<< v

instance applyDecoder :: Semigroup e => Apply (Decoder e i) where
  apply pre cont = Decoder $ \i -> (run pre i) <*> (run cont i)

instance applicativeDecoder :: Semigroup e => Applicative (Decoder e i) where
  pure a = Decoder $ const (pure a)

instance bindDecoder :: Semigroup e => Bind (Decoder e i) where
  bind pre cont = Decoder \i -> (run pre i) `V.andThen` \o -> run (cont o) i

instance monadDecoder :: Semigroup e => Monad (Decoder e i)

instance semigroupiodDecoder :: Semigroupoid (Decoder e) where
  compose g f = Decoder \i -> (run f i) `V.andThen` \o -> run g o

instance semigroupDecoder :: (Semigroup e, Semigroup o) => Semigroup (Decoder e i o) where
  append f g = Decoder \i -> (run f i) <> (run g i)

instance monoidDecoder :: (Monoid e, Monoid o) => Monoid (Decoder e i o) where
  mempty =  Decoder \_ -> mempty

instance altDecoder :: Alt (Decoder e i) where
  alt f g = Decoder \i -> V $ (V.toEither $ run f i) <|> (V.toEither $ run g i)

instance plusDecoder :: Plus m => Plus (Decoder (m e) i) where
  empty = fail empty

instance alternativeDecoder :: (Plus m, Semigroup (m e)) => Alternative (Decoder (m e) i)

-- | An alias for special decoder that does not produce any outputs.
-- | It is used for just validating inputs.
type Validator err input = Decoder err input Unit


-- Decode functions

-- | Basic function that decodes input by given decoder.
run :: forall e i o. Decoder e i o -> i -> V e o
run (Decoder f) a = f a

-- | Primitive decoder that always succeeds with input as it is.
identity :: forall e i. Semigroup e => Decoder e i i
identity = Decoder pure

-- | Primitive decoder that always succeeds with constant value.
always :: forall e i o. Semigroup e => o -> Decoder e i o
always a = Decoder $ const (pure a)

-- | Primitive decoder which always results to invalid.
fail :: forall e i o. e -> Decoder e i o
fail err = Decoder $ const (V.invalid err)

-- | Decoder into `Int`, raising `err` when a input is invalid for an integer.
int :: forall e. e -> Decoder e String Int
int err = Decoder $ V <<< Either.note err <<< Int.fromString

-- | Decoder into `Number`, raising `err` when a input is invalid for a number.
number :: forall e. e -> Decoder e String Number
number err = Decoder $ V <<< Either.note err <<< Num.fromString

-- | Decoder into given data type, raising `err` when a input is invalid for the data.
symbol :: forall e s. Semigroup e => e -> s -> String -> Decoder e String s
symbol err sybl str = Decoder \i -> if i == str then pure sybl else V.invalid err


-- Primitive validators

-- | Constructor for `Validator e i` takes a custom validation function.
validator :: forall e i. Semigroup e => e -> (i -> Boolean) -> Validator e i
validator err rule =
  Decoder \val -> if rule val
    then pure unit
    else V.invalid err

-- | Primitive validator limiting by not null.
required :: forall e. Semigroup e => e -> Validator e String
required err = validator err (not <<< Str.null)

-- | Primitive validator limiting by minimum bound.
minBound :: forall e ord. Semigroup e => Ord ord => e -> ord -> Validator e ord
minBound err bound = validator err (_ >= bound)

-- | Primitive validator limiting by maximum bound.
maxBound :: forall e ord. Semigroup e => Ord ord => e -> ord -> Validator e ord
maxBound err bound = validator err (_ <= bound)

-- | Primitive validator limiting by minimum length.
minLength :: forall e. Semigroup e => e -> Int -> Validator e String
minLength err bound = validator err (\s -> Str.length s >= bound)

-- | Primitive validator limiting by maximum length.
maxLength :: forall e. Semigroup e => e -> Int -> Validator e String
maxLength err bound = validator err (\s -> Str.length s <= bound)


-- Helper functions to validation

-- | Apply validator on given decoder.
-- | If a input is invalid for given validator, decoding fails.
assert :: forall e i o. Semigroup e => Validator e i -> Decoder e i o -> Decoder e i o
assert v d = v >>= (\_ -> d)

-- | Only checks validity if a condition is `true`.
when :: forall e i. Semigroup e => (i -> Boolean) -> Validator e i -> Validator e i
when c v =
  Decoder \i -> if c i
    then run v i
    else pure unit

-- | Only checks validity unless a condition is `true`.
unless :: forall e i. Semigroup e => (i -> Boolean) -> Validator e i -> Validator e i
unless g = when (not <<< g)


-- Function to build up decoder for forms

-- | `hoist` is mainly used for accessing sub model of target value.
hoist :: forall i j e o. (j -> i) -> Decoder e i o -> Decoder e j o
hoist f (Decoder g) = Decoder (g <<< f)
