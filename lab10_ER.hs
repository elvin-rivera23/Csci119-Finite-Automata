-- Elvin Rivera
-- M 11-12:50

-- Lab 10: REG closure under other operations

-- Ordinary regular expressions and a show method for them
data RE  = Empty | Letter Char | Union RE RE | Cat RE RE | Star RE

instance (Show RE) where    -- use precedence to minimize parentheses
  showsPrec d Empty         = showString "@"
  showsPrec d (Letter c)    = showString [c]
  showsPrec d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                              showsPrec 6 r1 .
                              showString "+" .
                              showsPrec 6 r2
  showsPrec d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                              showsPrec 7 r1 .
                              showsPrec 7 r2
  showsPrec d (Star Empty)  = showString "1"
  showsPrec d (Star r1)     = showsPrec 9 r1 .     -- prec(Star) = 8
                              showString "*"

-- Sigma = [a,b] for testing, but must work for any finite list
sigma :: [Char]
sigma = "ab"


-- String to RE
toRE :: String -> RE
toRE w = toRE' w [] where
  toRE' [] [r] = r
  toRE' ('+':xs) (r2:r1:rs) = toRE' xs (Union r1 r2:rs)
  toRE' ('.':xs) (r2:r1:rs) = toRE' xs (Cat r1 r2:rs)
  toRE' ('*':xs) (r:rs) = toRE' xs (Star r:rs)
  toRE' ('@':xs) rs = toRE' xs (Empty:rs)
  toRE' (x:xs) rs = toRE' xs (Letter x:rs)
  
-- assuming success; taken from lab6
acc :: Eq a => [(a,Char,a)] -> a -> Char -> a
acc ((q1, a1, q2):ts) q a | q1 == q && a1 == a = q2
                         | otherwise = acc ts q a

-- string acceptance
delt :: Eq a => FSM a -> a -> Char -> a
delt = acc . delta

deltstr :: Eq a => FSM a -> a -> [Char] -> a
deltstr = foldl . delt

accept1 :: Eq a => FSM a -> [Char] -> Bool 
accept1 m w = elem (deltstr m (start m) w) (finals m)

-- Finite state machines (as records), indexed by the type of their states
-- M = FSM {states=qs, start=s, finals=fs, delta=d}, and a show instance.
data FSM a = FSM {
  states :: [a],
  start  :: a,
  finals :: [a],
  delta  :: [(a,Char,a)]
  }

instance Show a => Show (FSM a) where
  show m = "("   ++ show (states m) ++
           ", "  ++ show (start m)  ++
           ", "  ++ show (finals m) ++
           ", [" ++ steps (delta m) ++ "])" where
    steps [] = []
    steps [t] = step t
    steps (t:ts) = step t ++ "," ++ steps ts
    step (q,c,q') = show q ++ "/" ++ [c] ++ ">" ++ show q'


-- Implement each of the following operations on regular expressions or FSMs

-- [[reverseRE r]] = rev([[r]]), defined by recursion on r
reverseRE :: RE -> RE
reverseRE Empty = Empty
reverseRE (Letter c) = (Letter c)
reverseRE (Union r1 r2) = Union (reverseRE r1) (reverseRE r2)
reverseRE (Cat r1 r2) = Cat (reverseRE r2)(reverseRE r1)
reverseRE (Star r) = Star (reverseRE r)

-- L(complementFSM M) = Sigma^* - L(M)
complementFSM :: Ord a => FSM a -> FSM a
complementFSM m = FSM {
                    states = (states m),
                    start = (start m),
                    finals = [x |x <- (states m), (x `elem` (finals m)) == False ],
                    delta = (delta m)
                    }

-- L(intersectFSM m1 m2) = L(m1) intersect L(m2)
intersectFSM :: (Ord a, Ord b) => FSM a -> FSM b -> FSM (a,b)
intersectFSM m1 m2 = FSM {
                       states = [(q1, q2) | q1 <- (states m1), q2 <- (states m2)],
                       start = (start m1, start m2),
                       finals = [(f1, f2) | f1 <- (finals m1), f2 <- (finals m2)],
                       delta = [((qm1, qm2), c, (dm1, dm2)) | (qm1, c, dm1)<-(delta m1), (qm2, c', dm2)<- (delta m2), c == c']
                       }

-- [[himage r h]] = h^*([[r]]), defined by recursion on r
himage :: RE -> (Char -> [Char]) -> RE
himage Empty _ = Empty
himage (Letter c) h = toRE (h c) -- toRE used for match  on error
himage (Union r1 r2) h = Union (himage r1 h) (himage r2 h)
himage (Cat r1 r2) h = Cat (himage r1 h) (himage r2 h)
himage (Star r) h = Star (himage r h)


-- L(hinvimage m h) = (h^*)^{-1}(L(m))
hinvimage :: Ord a => FSM a -> (Char -> [Char]) -> FSM a
hinvimage m h = FSM {
                  states = states m,
                  start  = start m,
                  finals  = finals m,
                  delta = [(q, a, (deltstr m (start m) [ha | ha <- (h a), elem ha sigma])) | q <- (states m), a <- sigma]
                  }

-- L(rightq m a) = { w | wa in L(m) }
rightq :: Ord a => FSM a -> Char -> FSM a
rightq m a = FSM{
               states = states m,
               start  = start m,
               finals = [deltstr m (start m) [a] | (deltstr m (start m) [a]) `elem` (finals m)],
               delta = delta m
               }

-- Note: left quotient was already implemented in a previous lab
