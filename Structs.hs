{-# LANGUAGE OverloadedStrings #-}

module Structs where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Array

data Direction = LEFT | RIGHT | UP | DOWN
    deriving(Show)

data Register = ACC | Dir Direction

instance Show Register where
    show ACC = "ACC"
    show (Dir dir) = show dir

data Operand = Imm Int | Reg Register

instance Show Operand where
    show (Imm i) = show i
    show (Reg reg) = show reg

data Instruction =
      NOP
    | MOV Operand Operand
    | SWP
    | SAV
    | ADD Operand
    | SUB Operand
    | NEG
    | JMP Label
    | JEZ Label
    | JNZ Label
    | JGZ Label
    | JLZ Label
    | JRO Operand
    deriving(Show)

data Comment = Comment Text
    deriving(Show)

data Label = Label Text

instance Show Label where
    show (Label label) = T.unpack label


data Line = Line (Maybe Label) (Maybe Instruction) (Maybe Comment)
    deriving(Show)

data CellProgram = Cell [Line]
    deriving(Show)

data Program = Program [CellProgram]
    deriving(Show)

data Cell = Compute | Memory | Damaged
    deriving(Show)

data Input = Input Text
    deriving(Show)

data Output = Output Text
    deriving(Show)

data Machine = Machine (Array (Int, Int) Cell) (Array Int (Maybe Input)) (Array Int (Maybe Output))
    deriving(Show)

printLine (Line label instr comment) =
    let label_str (Label label) = label <> ":"
        comment_str (Comment comment) = "# " <> comment
    in
        T.unwords $ catMaybes [label_str <$> label, (T.pack . show) <$> instr, comment_str <$> comment]

