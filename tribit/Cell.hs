module Cell where

-- Two-valued data placeholder pyramid cells.
data Cell = Zero | One deriving (Eq, Show)

-- Reports a character representation of a cell.
cell2char :: Cell -> Char
cell2char Zero = '0'
cell2char One  = '1'

-- Reports a cell representation of a character.
char2cell :: Char -> Cell
char2cell c = case c of
    '0' -> Zero
    '1' -> One
    _   -> error "Can only convert '0' or '1' to cell." 
