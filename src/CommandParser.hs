module CommandParser (Instruction(..), Command(..), parseCommand, commands) where

import           Data.Foldable
import           Mod
import           Text.ParserCombinators.Parsec

-- Verbs that describe an action on a given Setting
data Instruction = Add
                 | Remove
                 | Set
                 | Toggle
                 deriving (Eq, Show)

-- Instructions and all their aliases
instructionAliases :: [(Instruction, [String])]
instructionAliases = [(Add, ["add", "+"]), (Set, ["set", "="]), (Remove, ["remove", "-"])]

-- A type that represents a command input
data Command = Command Instruction String String Setting -- Instruction, mod name, setting name, setting value
             deriving (Show, Eq)

-- Generate a parser to match a member of a list
parseOneWith :: [a] -> (a -> Parser a) -> Parser a
parseOneWith xs p = asum . fmap p $ xs

-- Parse a command instruction, verbs that say what to do to the settings
-- Toggle in the case that there is no verb
instructionParser :: Parser Instruction
instructionParser = spaces *> option Toggle instructions
  where
    parseInstruction i xs = i <$ try (parseOneWith xs string)
    instructions = asum . fmap (uncurry parseInstruction) $ instructionAliases

-- Parse out a Mod instance from a list using its name string
modParser :: [Mod] -> Parser Mod
modParser mods = parseOneWith mods parserForMod
  where
    parserForMod :: Mod -> Parser Mod
    parserForMod m@(Mod name _ _) = m <$ (spaces *> string name)

-- Parse an int value into a setting
intParser :: Parser Setting
intParser = Int . read <$> (spaces *> many1 digit)

-- Parse a bool value into a setting
boolParser :: Parser Setting
boolParser = Bool . (== "true") <$> (string "true" <|> string "false")

-- Parse a float value into a setting
floatParser :: Parser Setting
floatParser = Float . read <$> (spaces *> many1 digit <++> string "." <++> many1 digit)
  where
    -- Concatenate two string parsers
    (<++>) :: Applicative f => f [a] -> f [a] -> f [a]
    (<++>) a b = (++) <$> a <*> b

-- Parse out a single word
wordParser :: Parser String
wordParser = many1 alphaNum

-- Parse any setting type
settingParser :: Parser Setting
settingParser = try boolParser <|> try floatParser <|> intParser <|> (String <$> wordParser) <|> listParser

-- Parse a list of some setting type
listParser :: Parser Setting
listParser = List <$> (spaces *> between (char '[') (char ']') (sepBy1 settingParser spaces))

-- Parse a setting name given a list of mod settings
modSettingsParser :: [(String, Setting)] -> Parser String
modSettingsParser s = spaces *> parseOneWith (fst <$> s) string

-- Parse a command with no instructions
toggleCommandParser :: [Mod] -> Parser Command
toggleCommandParser mods = do
  (Mod name _ _) <- modParser mods
  return (Command Toggle name "" (Float 0))

-- Parse a command that includes instructions
verbCommandParser :: [Mod] -> Parser Command
verbCommandParser mods = do
  (Mod name settings _) <- modParser mods
  settingName           <- modSettingsParser settings <* string " "
  instruction           <- instructionParser
  Command instruction name settingName <$> (spaces *> settingParser)

-- Parse a command structure
commandParser :: [Mod] -> Parser Command
commandParser mods = verbCommandParser mods <|> toggleCommandParser mods

nuker = Mod "nuker" [("speed", Float 1)] False
fly = Mod "fly" [("nokick", Bool True), ("speed", Float 1)] False
commands = [nuker, fly, Mod "xray" [("blocks", List [])] False]


parseCommand :: [Mod] -> String -> Either ParseError Command
parseCommand mods input = parse (commandParser mods) "command" input

t i = case result of
  Left e  -> print e
  Right r -> print r
  where result = parse (commandParser commands) "commands" i
