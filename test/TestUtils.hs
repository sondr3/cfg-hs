module TestUtils (testParse, liftEither) where

import Text.Megaparsec

-- | Test utility to run a parser on some input
testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse p = parse p ""

-- | Only ever used in tests.
liftEither :: Either a p -> p
liftEither (Right r) = r
liftEither (Left _) = error "liftEither called with Left"
