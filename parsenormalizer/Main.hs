module Main where
import System.Environment(getArgs)
import Lambda (Term(..), normal') 
import Text.ParserCombinators.Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token

TokenParser { parens = parens
            , identifier = identifier
            , reserved = reserved
            , reservedOp = reservedOp
            , whiteSpace = whiteSpace } = makeTokenParser haskellDef

main = do
        args  <- getArgs
        input <- readFile $ head args
        case parse (many parseTerm) "" input of
            Right terms -> foldr ((>>) . putStrLn . convert False . normal') (return()) terms 
            Left error  -> print error

appList :: Term -> [Term] -> Term
appList = foldl (\t1 t2  -> App t1 t2)

absList :: [String] -> Term -> Term
absList vars term = foldr (\var t -> Abs var t) term vars

parseLet = do {
	Main.reserved "let"
	;id <- Main.identifier
	;Main.reservedOp "="
	;letTerm <- parseTerm
	;Main.reserved "in"
	;inTerm <- parseTerm
    ;return $ App $ Abs id inTerm letTerm
}

parseLambda :: Parser Term
parseLambda = 
            do {
				Main.reservedOp "\\"
				;ids <- many1 Main.identifier
				;(Main.reservedOp ".") <|> (Main.reservedOp "->")
				;term <- parseTerm
				;return $ absList ids term
			}
		    <|> do {
		        term <- parseSimpleTerm
                ; terms <- many parseSimpleTerm
                ; return $ appList term terms
			}			

parseSimpleTerm :: Parser Term
parseSimpleTerm = do{ id <- Main.identifier
					  ; return $ Var id
				  }
				  <|> Main.parens parseLambda

parseTerm :: Parser Term
parseTerm = parseLet <|> parseLambda
			
convert :: Bool -> Term -> String
convert isInParens term = case isInParens of
							False   -> case term of
										(Abs var term') -> "\\" ++ var ++ " -> " ++ (convert False term')
										(App t1 t2)     -> (convert True t1) ++ " " ++ (convert True t2)
										(Var v)         -> v
							True    -> case term of
										(Var v)  		-> v
										term' 			-> "(" ++ (convert False term') ++ ")"
