-- compile with "-p contrib" argument
import Text.Lexer
import Text.Parser

{-
some example
this is json:
{
    "info" : {
        "name" : "zdc",
        "age" : 20,
        "enrolls" : ["math", "cs"]
    },
    "info1" : true
}
用一個ADT最好表示了啦
-}

data Json = JsonNum Integer
            | JsonDouble Double
            | JsonString String
            | JsonBool Bool
            | JsonList (List Json)
            | JsonObject (List (String, Json))
-- actually we can do lexical analysis and parsing together, but, to get acquintances with idris Lexer/Parse library...
data Token = LeftCurly | RightCurly | LeftBracket | RightBracket | Quote | Colon | Comma
            | Str String | Boolean Bool | NumInt Int | NumDouble Double

-- haven't learned how to process whitespaces
toks : TokenMap Token
toks =
    [
        (((range '1' '9' <+> digits) <|> digits) <+> is '.' <+> digits, \x => NumDouble (cast x)),
        ((range '1' '9' <+> digits) <|> digits, \x => NumInt (cast x)),
        (quote (is '"') any, \x => Str (pack (reverse $ drop 1 $ reverse $ drop 1 (unpack x)))), -- very ugly
        (exact "true", \x => Boolean True),
        (exact "false", \x => Boolean False),
        (is '[', \x => LeftBracket),
        (is ']', \x => RightBracket),
        (is '{', \x => LeftCurly),
        (is '}', \x => RightCurly),
        (is '"', \x => Quote),
        (is ',', \x => Comma),
        (is ':', \x => Colon)
    ]