-- | A comprehensive list of standard Haskell Prelude exports to check for collisions in the haskell compiler

module Language.Sparcl.Compiler.PreludeExports where

preludeExports :: [String]
preludeExports =
    [ "id", "const", "flip", "curry", "uncurry", "fst", "snd", "until", "asTypeOf", "error"
    , "undefined", "seq", "($)", "($!)", "(&&)", "(||)", "not", "otherwise", "maybe"
    , "either", "compare", "max", "min", "succ", "pred", "toEnum", "fromEnum"
    , "enumFrom", "enumFromThen", "enumFromTo", "enumFromThenTo", "minBound", "maxBound"
    , "(==)", "(/=)", "(<)", "(<=)", "(>)", "(>=)", "(+)", "(-)", "(*)", "negate", "abs"
    , "signum", "fromInteger", "toRational", "quot", "rem", "div", "mod", "quotRem", "divMod"
    , "toInteger", "(/)", "recip", "fromRational", "pi", "exp", "log", "sqrt", "(**)"
    , "logBase", "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh"
    , "asinh", "acosh", "atanh", "properFraction", "truncate", "round", "ceiling", "floor"
    , "floatRadix", "floatDigits", "floatRange", "decodeFloat", "encodeFloat", "exponent"
    , "significand", "scaleFloat", "isNaN", "isInfinite", "isDenormalized", "isNegativeZero"
    , "isIEEE", "atan2", "subtract", "even", "odd", "gcd", "lcm", "(^)", "(^^)", "fromIntegral"
    , "realToFrac", "map", "(++)", "filter", "head", "last", "tail", "init", "null", "length"
    , "(!!)", "reverse", "foldl", "foldl1", "foldr", "foldr1", "and", "or", "any", "all", "sum"
    , "product", "concat", "concatMap", "maximum", "minimum", "scanl", "scanl1", "scanr"
    , "scanr1", "iterate", "repeat", "replicate", "cycle", "take", "drop", "splitAt", "takeWhile"
    , "dropWhile", "span", "break", "elem", "notElem", "lookup", "zip", "zip3", "zipWith"
    , "zipWith3", "unzip", "unzip3", "lines", "words", "unlines", "unwords", "show", "read"
    , "reads", "shows", "readParen", "lex", "showsPrec", "showList", "readsPrec", "readList"
    , "showChar", "showString", "showParen", "ioError", "userError", "catch", "return", "(>>=)"
    , "(>>)", "fail", "fmap", "(<$>)", "pure", "(<*>)", "(<*)", "(*>)", "mempty", "mappend"
    , "mconcat", "foldMap", "toList", "traverse", "sequenceA", "mapM", "sequence", "(.)"
    -- Data types and constructors:
    , "Maybe", "Just", "Nothing", "Either", "Left", "Right", "Bool", "True", "False"
    , "Ordering", "LT", "EQ", "GT", "String", "Char", "Int", "Integer", "Float", "Double"
    , "Rational", "IO", "Eq", "Ord", "Enum", "Bounded", "Num", "Real", "Integral", "Fractional"
    , "Floating", "RealFrac", "RealFloat", "Monad", "Functor", "Applicative", "Show", "Read"
    ]