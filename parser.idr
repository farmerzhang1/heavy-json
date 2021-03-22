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
data Token = LeftCurly | RightCurly | LeftBracket | RightBracket | Colon | Comma | Comment String
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
        (is ':', \x => Colon),
        (spaces, Comment)
    ]

processWhitespace : (List (TokenData Token), Int, Int, String) -> (List (TokenData Token), Int, Int, String)
processWhitespace (x,l,c,s) = ((filter notComment x),l,c,s) where
    notComment : TokenData Token -> Bool
    notComment t = case tok t of
                        Comment _ => False
                        _ => True

Rule : Type -> Type
Rule ty = Grammar (TokenData Token) True ty

{-
json = { json_object }
json_object = json_string : json_values (, json_string : json_values )****
json_values = listof json_values | Integer | double | bool | string | json_object
-}

-- this is too verbose, can I simplify it?
left_bracket : Rule Integer
left_bracket = terminal (\x => case tok x of
                            LeftBracket => Just 0
                            _ => Nothing)

right_bracket : Rule Integer
right_bracket = terminal (\x => case tok x of
                            RightBracket => Just 0
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
comma : Rule Integer
comma = terminal (\x => case tok x of
                        Comma => Just 0
                        _ => Nothing)
string : Rule String
string = terminal (\x => case tok x of
                        Str s => Just s
                        _ => Nothing)
jstring : Rule Json
jstring = terminal (\x => case tok x of
                        Str s => Just $ JsonString s
                        _ => Nothing)
jnum : Rule Json
jnum = terminal (\x => case tok x of
                    NumInt n => Just $ JsonNum n
                    _ => Nothing)
jdouble : Rule Json
jdouble = terminal (\x => case tok x of
                        NumDouble d => Just $ JsonDouble d
                        _ => Nothing)
jbool : Rule Json
jbool = terminal (\x => case tok x of
                        Boolean b => Just $ JsonBool b
                        _ => Nothing)

json_array : Rule Json
json_list : Rule Json
json_values : Rule Json
json_values = jstring <|> jnum <|> jdouble <|> jbool <|> json_array <|> json_list
-- MONAD!!
-- single json object with one string key, one json value
-- "name" : some json values (string, bool, number, list)
single_object : Rule (String, Json)
single_object = do
    s <- string
    colon
    val <- json_values
    pure (s, val)

json_list = do
    left_curly
    jlist <- sepBy1 comma single_object
    right_curly
    pure (JsonObject jlist)

json_array = do
    left_bracket
    jarray <- sepBy comma json_values
    right_bracket
    pure (JsonList jarray)

test : String -> Either (ParseError (TokenData Token))
    (Json, List (TokenData Token))
test s = parse json_list (fst (processWhitespace $ lex toks s))