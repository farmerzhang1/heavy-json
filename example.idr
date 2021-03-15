import Text.Lexer
import Text.Parser

data ExpressionToken = Number Integer
         | Operator String
         | OParen
         | CParen
         | EndInput

Rule : Type -> Type
Rule ty = Grammar (TokenData ExpressionToken) True ty

openParen : Rule Integer
openParen = terminal (\x => case tok x of
                    OParen => Just 0
                    _ => Nothing)

closeParen : Rule Integer
closeParen = terminal (\x => case tok x of
                    CParen => Just 0
                    _ => Nothing)

paren : Rule Integer -> Rule Integer
paren exp = openParen *> exp <* closeParen -- applicative

intLiteral : Rule Integer
intLiteral
  = terminal (\x => case tok x of
                  Number i => Just i
                  _ => Nothing)

expr1 : Rule Integer
expr1 = intLiteral <|> (paren expr1)

expr : Rule Integer
expr = intLiteral <|> do
    openParen
    r <- expr
    closeParen
    pure r

opChars : String
opChars = "+-*"

operator : Lexer
operator = some (oneOf opChars)

toInt' : String -> Integer
toInt' = cast

expressionTokens : TokenMap ExpressionToken
expressionTokens =
   [(digits, \x => Number (toInt' x)),
   (operator, \x => Operator x),
   (is '(' ,\x => OParen),
   (is ')' ,\x => CParen)]

test : String -> Either (ParseError (TokenData ExpressionToken))
                        (Integer, List (TokenData ExpressionToken))
test s = parse expr (fst (lex expressionTokens s))
