module Lesson04 exposing (main)

import Utils exposing (print)

type Parser a = Parser (String -> Maybe (a, String))

parse : Parser a -> String -> Maybe (a, String)
parse (Parser f) s = f s

fail : Parser a
fail = Parser (always Nothing)

eof : Parser ()
eof = Parser (\s -> if String.isEmpty s then Just ((), "") else Nothing)

pure : a -> Parser a
pure x = Parser (\s -> Just (x, s))

satisfy : (Char -> Bool) -> Parser Char
satisfy pred =
    Parser
        (\s -> case String.uncons s of
                   Nothing -> Nothing
                   Just (x, rest) ->
                       if pred x
                       then Just (x, rest)
                       else Nothing
        )

char : Char -> Parser Char
char = satisfy << (==)

anyChar : Parser Char
anyChar = satisfy (always True)

digit : Parser Char
digit = satisfy Char.isDigit

--

map : (a -> b) -> Parser a -> Parser b
map f pa = Parser (\s -> case parse pa s of
                             Nothing -> Nothing
                             Just (x, rest) -> Just (f x, rest)
                  )

ap : Parser (a -> b) -> Parser a -> Parser b
ap pf pa = Parser (\s -> case parse pf s of
                             Nothing -> Nothing
                             Just (f, rest) -> parse (map f pa) rest
                  )

join : Parser (Parser a) -> Parser a
join pa = Parser (\s -> case parse pa s of
                            Nothing -> Nothing
                            Just (p, rest) -> parse p rest
                 )

andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f = join << map f

or : Parser a -> Parser a -> Parser a
or p1 p2 = Parser (\s -> case parse p1 s of
                             Nothing -> parse p2 s
                             res -> res
                  )

-- quantifiers

some : Parser a -> Parser (List a)
some p = p |> andThen (\x -> map ((::) x) <| many p)

many : Parser a -> Parser (List a)
many p = or (some p) (pure [])

-- threading DSL

keep : Parser a -> Parser (a -> b) -> Parser b
keep pa pf = ap pf pa

skip : Parser a -> Parser b -> Parser b
skip pa pb = pure always |> keep pb |> keep pa

-- Example

validate : (a -> Maybe b) -> Parser a -> Parser b
validate v p = p |> andThen
                   (\x ->
                        case v x of
                            Nothing -> fail
                            Just y -> pure y
                   )

int : Parser Int
int =
    let absVal = validate (String.toInt << String.fromList) (some digit)
    in or (pure negate
          |> skip (char '-')
          |> keep absVal
          )
        absVal

string : String -> Parser String
string s =
    case String.uncons s of
        Nothing -> pure ""
        Just (c, cs) ->
            pure String.cons
                |> keep (char c)
                |> keep (string cs)

spaces = many (satisfy ((==) ' '))

tuple : Parser a -> Parser b -> Parser (a, b)
tuple pa pb =
    between (char '(' |> skip spaces) (spaces |> skip (char ')'))
      (pure Tuple.pair
        |> keep pa
        |> skip spaces
        |> skip (char ',')
        |> skip spaces
        |> keep pb)

between : Parser left -> Parser right -> Parser a -> Parser a
between l r p =
    pure identity
        |> skip l
        |> keep p
        |> skip r

sepBy : Parser sep -> Parser a -> Parser (List a)
sepBy s p = or (sepBy1 s p) (pure [])

sepBy1 : Parser sep -> Parser a -> Parser (List a)
sepBy1 s p =
    pure (::)
        |> keep p
        |> keep (many (pure identity |> skip s |> keep p))

listOf : Parser a -> Parser (List a)
listOf =
    between (char '[' |> skip spaces) (spaces |> skip (char ']'))
        << sepBy (pure identity
                 |> skip spaces
                 |> keep (char ',')
                 |> skip spaces
                 )

main = print <| parse (listOf (tuple (listOf int) int)) "[ ([3],    5) , ([], 0), ([ -456, 1000],  7) ]"
