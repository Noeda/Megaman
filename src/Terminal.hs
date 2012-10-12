module Terminal(Terminal, emptyTerminal, handleChar, printOut) where

data Terminal = Terminal { chars :: Int }

emptyTerminal :: Int -> Int -> Terminal
emptyTerminal _ _ = Terminal { chars = 0 }

handleChar :: Terminal -> Char -> Terminal
handleChar t@(Terminal chars) _ = t { chars = chars + 1 }

-- For debugging
printOut :: Terminal -> IO ()
printOut (Terminal chars) = print chars

