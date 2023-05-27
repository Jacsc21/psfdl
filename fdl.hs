import Graphics.HGL

-- state represented by list of letters
type State = [Char]

-- rule represented by a letter (left-hand side) and a state (right-hand side)
data Rule = Rule Char State deriving Show

-- turtle graphics commands (Nop is no operation)
data Command = Forward | LeftTurn Int | RightTurn Int | Nop | Scale Double | Backward deriving Show

-- turtle represented by reference to window, current position, current angle, current length
type Turtle = (Window, Double, Double, Double, Double)

-- fractal represented by initial state, list of rules, mapping from letters to commands, target depth, initial length
type Fractal = (State, [Rule], Char -> Command, Int, Double)

-- examples for rule lists
rules1 = [Rule 'F' "FLFRFLF"]
rules2 = [Rule 'X' "XRYF", Rule 'Y' "FXLY"]

-- examples for fractals
fractal1 = ("F", rules1, let m 'F' = Forward; m 'L' = LeftTurn 60; m 'R' = RightTurn 120 in m)
fractal2 = ("FX", rules2, let m 'F' = Forward; m 'L' = LeftTurn 90; m 'R' = RightTurn 90; m _ = Nop in m)

-- go from depth n to depth n+1
apply :: State -> [Rule] -> State
apply [] _ = []
apply (x:xs) [Rule] = applyRule x [Rule] ++ apply xs [Rule]

-- aplies the rules to a single character recursively
applyRule :: Char -> [Rule] -> State
applyRule x [] = [x]  
applyRule x (Rule targetCharacter replacement : [Rule])
  | x == targetCharacter = replacement
  | otherwise = applyRule x [Rule]

-- expand to target depth
expand :: State -> [Rule] -> Int -> State
expand input rules int = expandToDepth input [Rule] int 0

-- expands from depth n to depth n+1 recursively until it target depth is reached
expandToDepth :: State -> [Rule] -> Int -> Int -> State
expandToDepth input [Rule] targetDepth currentDepth
  | currentDepth == targetDepth = input
  | otherwise = expandToDepth (apply input [Rule]) [Rule] targetDepth (currentDepth+1)

-- expands the fractal to the target depth with the expand function
expandFractal :: Fractal -> State
expandFractal fractal = expand state [Rule] depth
  where
    state = getState fractal
    [Rule] = getRule fractal
    depth = getDepth fractal
  
-- convert fractal into sequence of turtle graphics commands
process :: Fractal -> [Command]
process fractal =
  let expandedState = expandFractal fractal
      mapping = getMapping fractal
  in processHelper mapping expandedState

-- maps the characters in the target state to the given mappings
processHelper :: (Char -> Command) -> State -> [Command]
processHelper = map

-- retrieves the start state of a given fractal
getState :: Fractal -> State
getState (state, _, _, _, _) = state

-- retrieves the rules of a given fractal
getRule :: Fractal -> [Rule]
getRule (_, [Rule], _, _, _) = [Rule]

-- retrieves the target depth of a given fractal
getDepth :: Fractal -> Int
getDepth (_, _, _, depth, _) = depth

-- retreives the mapping of a given fractal
getMapping :: Fractal -> Char -> Command
getMapping (_, _, mapping, _, _) = mapping

-- helper function to go from two floating point values to a pair of integers
toPoint :: Double -> Double -> Point
toPoint x y = (round x, round y)

-- main turtle graphics drawing function
drawIt :: Turtle -> [Command] -> IO ()
drawIt _                     []                    = return ()
drawIt turtle                (Nop:xs)              = drawIt turtle xs
drawIt (w, x, y, angle, len) (Forward:xs)          = let
  x' = x+len*(cos angle)
  y' = y-len*(sin angle)
  pen = createPen Solid 2 0 0 255
  in do
    withPen pen (line (toPoint x y) (toPoint x' y'))
    drawInWindow w (line (toPoint x y) (toPoint x' y'))
    drawIt (w, x', y', angle, len) xs
drawIt (w, x, y, angle, len) (Backward:xs)          = let
  x' = x-len*(cos angle)
  y' = y+len*(sin angle)
  in do
    drawInWindow w (line (toPoint x y) (toPoint x' y'))
    drawIt (w, x', y', angle, len) xs
drawIt (w, x, y, angle, len) (LeftTurn degree:xs)  = let angle' = angle+(fromIntegral degree)*pi/180 in
    drawIt (w, x, y, angle', len) xs
drawIt (w, x, y, angle, len) (RightTurn degree:xs) = let angle' = angle-(fromIntegral degree)*pi/180 in
    drawIt (w, x, y, angle', len) xs
drawIt (w, x, y, angle, len) (Scale factor:xs)     = let len' = len*factor in
    drawIt (w, x, y, angle, len') xs

-- draw a fractal by opening a window and using turtlegraphics
drawFractal fractal@(_, _, _, _, len) = runGraphics (withWindow_ "L-System using Turtle Graphics" (1000, 600)
        (\ w -> do
         drawIt (w, 500, 300, 0, len) (process fractal)
         getKey w))

-- create a list of lists of string, where each list contains the "words" of one line
split :: String -> String -> [String] -> [[String]]
split []        [] []  = []
split ('\n':xs) [] []  = split xs [] []
split ('\n':xs) [] zss = split xs [] []
split ('\n':xs) ys zss = reverse (reverse ys : zss) : split xs [] []
split (' ':xs)  [] zss = split xs [] zss
split (' ':xs)  ys zss = split xs [] (reverse ys:zss)
split (x:xs)    ys zss = split xs (x : ys) zss

-- interpret the list of lists as a description of initial state, rules, mapping, depth, and length of a fractal
interpret :: [[String]] -> State -> [Rule] -> (Char -> Command) -> Int -> Double -> Fractal
interpret []                   start rules m depth len = (start, rules, m, depth, len)
interpret (("rule":xs):yss)    start rules m depth len = interpret yss start (rules ++ [interpretRule xs]) m depth len where
  interpretRule (l:"->":rs) = Rule (head l) (foldr (++) [] rs)
interpret (("start":xs):yss)   _     rules m depth len = interpret yss (foldr (++) [] xs) rules m depth len
interpret (("cmd":xs):yss)     start rules m depth len = interpret yss start rules (interpretCmd xs m) depth len where
  interpretCmd [c, "fd"]            m = \x -> if x == (head c) then Forward else m x
  interpretCmd [c, "lt", deg]       m = \x -> if x == (head c) then LeftTurn (read deg) else m x
  interpretCmd [c, "rt", deg]       m = \x -> if x == (head c) then RightTurn (read deg) else m x
  interpretCmd [c, "nop"]           m = \x -> if x == (head c) then Nop else m x
  interpretCmd [c, "scale", factor] m = \x -> if x == (head c) then Scale (read factor) else m x
  interpretCmd [c, "bk"]            m = \x -> if x == (head c) then Backward else m x
interpret (("length":arg:xs):yss) start rules m depth _   = interpret yss start rules m depth (read arg)
interpret (("depth":arg:xs):yss)  start rules m _     len = interpret yss start rules m (read arg) len

-- read from given file and return a fractal
readFractal fileName = do
  text <- readFile fileName
  return (interpret (split text [] []) [] [] (\x -> error "unknown command") 0 0)

-- daw a fractal described in an .fdl file
drawFdl fileName = do
  fractal <- readFractal fileName
  drawFractal fractal

-- main function that draws the snowflake fractal
main :: IO ()
main = drawFdl "snowflake.fdl"
