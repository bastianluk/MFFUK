-- Typ reprezentující operaci uvnitř typu Exp
data Op = Plus | Minus | Times 
  deriving (Eq, Ord)
instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"

-- Typ reprezentující strom výpočtu. V listech máme celočíselnou konstantu, ve vnitřních vrcholech operace
data Exp = Const Int | Oper Op Exp Exp
  deriving (Eq, Ord)
instance Show Exp where
  show (Const i) = show i
  show (Oper op e1 e2) = showInner e1 ++ " " ++ show op ++ " " ++ showInner e2
-- Zpusob jak obejit uzavorkovani cele expression.
showInner (Const i) = show i
showInner (Oper op e1 e2) = "(" ++ showInner e1 ++ " " ++ show op ++ " " ++ showInner e2 ++ ")"

arit :: [Int] -> Int -> [Exp]
arit list result = do i <- [1..length list-1]
                      let (subl, subr) = splitAt i list
                      expl <- gen subl
                      expr <- gen subr
                      op <- [Plus, Minus, Times]
                      if evalExpression (Oper op expl expr) == result then
                          return (Oper op expl expr)
                      else []

gen :: [Int] -> [Exp]
gen (x:[]) = [(Const x)]
gen list = do i <- [1..length list-1]
              let (subl, subr) = splitAt i list
              expl <- gen subl
              expr <- gen subr
              op <- [Plus, Minus, Times]
              return (Oper op expl expr)

evalExpression :: Exp -> Int
evalExpression (Oper Plus exp1 exp2) = (evalExpression exp1) + (evalExpression exp2)
evalExpression (Oper Minus exp1 exp2) = (evalExpression exp1) - (evalExpression exp2)
evalExpression (Oper Times exp1 exp2) = (evalExpression exp1) * (evalExpression exp2)
evalExpression (Const int) = int