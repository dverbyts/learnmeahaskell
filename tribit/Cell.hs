module Cell where

data Cell = Zero | One deriving (Eq, Show)

cell2char :: Cell -> Char
cell2char Zero = '0'
cell2char One  = '1'

char2cell :: Char -> Cell
char2cell c = case c of
    '0' -> Zero
    '1' -> One
    _   -> error "Can only convert '0' or '1' to cell." 
