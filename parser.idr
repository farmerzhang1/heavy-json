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
data Token = LeftCurly | RightCurly | LeftBracket | RightBracket | Colon | Comma
            | Str String | Boolean Bool | NumInt Integer | NumDouble Double

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
        (is ',', \x => Comma),
        (is ':', \x => Colon)
    ]


Rule : Type -> Type
Rule ty = Grammar (TokenData Token) True ty

{-
json = { json_object }
json_object = json_string : json_values (, json_string : json_values )****
json_values = listof json_values | Integer | double | bool | string | json_object
-}


left_bracket : Rule Integer
left_bracket = terminal (\x => case tok x of
                            LeftBracket => Just 0
                            _ => Nothing)

right_bracket : Rule Integer
right_bracket = terminal (\x => case tok x of
                            LeftBracket => Just 0
                            _ => Nothing)
left_curly : Rule Integer
left_curly = terminal (\x => case tok x of
                            LeftCurly => Just 0
                            _ => Nothing)

right_curly : Rule Integer
right_curly = terminal (\x => case tok x of
                            RightCurly => Just 0
                            _ => Nothing)
colon : Rule Integer
colon = terminal (\x => case tok x of
                        Colon => Just 0
                        _ => Nothing)
jstring : Rule String
jstring = terminal (\x => case tok x of
                        Str s => Just s
                        _ => Nothing)

jnum : Rule Integer
jnum = terminal (\x => case tok x of
                    NumInt n => Just n
                    _ => Nothing)
-- MONAD!!
-- single json object with one string key, one integer value
single_object : Rule Json
single_object = do
    left_curly
    s <- jstring
    colon
    n <- jnum
    right_curly
    pure (JsonObject [(s, JsonNum n)])

test : String -> Either (ParseError (TokenData Token))
    (Json, List (TokenData Token))
test s = parse single_object (fst (lex toks s))