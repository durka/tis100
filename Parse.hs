{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec hiding (Label, label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad

import Structs

type Parser = Parsec Void Text

space' :: Parser ()
space' = skipMany $ choice [void spaceChar, void $ char ',']

register :: Parser Register
register = choice
    [ ACC       <$ string "ACC"
    , Dir LEFT  <$ string "LEFT"
    , Dir RIGHT <$ string "RIGHT"
    , Dir UP    <$ string "UP"
    , Dir DOWN  <$ string "DOWN"   ]

operand :: Parser Operand
operand = choice
    [ Imm <$> L.lexeme space L.decimal
    , do
        char '-'
        int <- L.lexeme space L.decimal
        return $ Imm (-int)
    , Reg <$> register ]

instruction :: Parser Instruction
instruction = choice
    [ NOP <$ string "NOP"
    , do
        string "MOV"
        space'
        src <- operand
        space'
        dst <- operand
        return $ MOV src dst
    , SWP <$ string "SWP"
    , SAV <$ string "SAV"
    , do
        string "ADD"
        space'
        src <- operand
        return $ ADD src
    , do
        string "SUB"
        space'
        src <- operand
        return $ SUB src
    , NEG <$ string "NEG"
    , do
        string "JMP"
        space'
        lbl <- label
        return $ JMP lbl
    , do
        string "JEZ"
        space'
        lbl <- label
        return $ JEZ lbl
    , do
        string "JNZ"
        space'
        lbl <- label
        return $ JNZ lbl
    , do
        string "JLZ"
        space'
        lbl <- label
        return $ JLZ lbl
    , do
        string "JGZ"
        space'
        lbl <- label
        return $ JGZ lbl
    , do
        string "JRO"
        space'
        src <- operand
        return $ JRO src        ]
    <?> "instruction"

comment :: Parser Comment
comment = do
    char '#'
    text <- manyTill printChar $ choice [eof, void eol]
    return $ Comment (T.pack text)

label :: Parser Label
label = do
    name <- some alphaNumChar
    return $ Label (T.pack name)

line :: Parser Line
line = do
    label <- optional $ do
        lbl <- label
        char ':'
        return lbl
    space'
    instruction <- optional instruction
    space'
    comment <- optional comment
    return $ Line label instruction comment

