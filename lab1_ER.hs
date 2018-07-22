--- Elvin Rivera	M 11-12:50
--- Studnet ID: 107027563

---- CSci 119, Lab 1 ----

-- Note: you will replace all instances of "undefined" below with your answers.


---- Boolean operators

-- The following code tests whether "and" is commutative:
bools = [True, False]
and_commutes = and [(p && q) == (q && p) | p <- bools, q <- bools]

-- Write similar defintions that test whether "or" is commutative,
-- "and" and "or" are associative, "and" distributes over "or",
-- "or" distributes over "and"
or_commutes = and  [(p || q) == (q || p) | p <- bools, q <- bools]
and_assoc = and  [((p && q) && r) == (p && (q && r)) | p <- bools, q <- bools, r <- bools]
or_assoc = and  [((p || q) || r) == (p || (q || r)) | p <- bools, q <- bools, r <- bools]
and_dist = and [(p && (q || r)) == ((p && q) || (p && r)) | p <- bools, q <- bools, r <- bools]
or_dist = and [(p || (q && r)) == ((p || q) && (p || r)) | p <- bools, q <- bools, r <- bools]
          
-- The exclusive-or operation on Bool in Haskell is equivalent to /=.
-- Test the properties of this operation (commutativity, associativity,
-- distributivity over and+or, and distributivity of and+or over it)
-- using the same method as above
xor_commutes = and  [(p /= q) == (q /= p) | p <- bools, q <- bools]
xor_assoc    = and  [((p /= q) /= r) == (p /= (q /= r)) | p <- bools, q <- bools, r <- bools]
xor_dist_and = and  [(p /= (q && r)) == ((p /= q) && (p /= r)) | p <- bools, q <- bools, r <- bools]
xor_dist_or  = and  [(p /= (q && r)) == ((p /= q) && (p /= r)) | p <- bools, q <- bools, r <- bools]
and_dist_xor = and  [(p && (q /= r)) == ((p && q) /= (p && r)) | p <- bools, q <- bools, r <- bools]
or_dist_xor  = and  [(p || (q /= r)) == ((p || q) /= (p || r)) | p <- bools, q <- bools, r <- bools]
               
-- The implication operator on Bool in Haskell is equivalent to <=.
-- Check whether implication is associative or commutative:
assoc_imp = and [((p <= q) <= r) == (p <= (q <= r)) | p <- bools, q <- bools, r <- bools]
comm_imp = and [(p <= q) == (q <= p) | p <- bools, q <- bools]


----- Evaluating statements involving quantifiers

-- Assume that the universe of discourse is the set {1,2,3,4,5,6,7,8},
-- that is, that the word "number" temporarily means 1, 2, ..., 8.

u = [1..8]

-- Translate each of the following statements first, in a comment, into a
-- logical statement involving forall, exists, and, or, imp, and not,
-- and then into Haskell code that checks ("brute force") whether
-- the statement is true. I'll work one example.

-- 1. "Every number that's greater than 2 is greater than 1"
-- A: forall n, (n > 2) imp (n > 1)
prob1 = and [(n > 2) <= (n > 1) | n <- u]

-- 2. Every number is either greater than 1 or less than 2
-- A: 
prob2 = and [(n > 1) || (n < 2) | n <-u]

-- 3. Every two numbers are comparable with <= (i.e., either one is <=
--    the other or vice-versa)
-- A: 
prob3 = and [(n1 <= n2) || (n2 <= n1) | n1 <- u, n2 <- u]

-- 4. For every odd number, there is a greater even number (use the Haskell
--    predicates odd, even :: Integral a => a -> Bool)
-- A: 
prob4 = and [or [n2 > n1 | n2 <- u, even n2] | n1 <- u, odd n1]

-- 5. For every even number, there is a greater odd number
-- A: 
prob5 = and [or [n2 > n1 | n2 <- u, odd n2] | n1 <- u, even n1]

-- 6. There are two odd numbers that add up to 6
-- A: 
prob6 = or [n1 + n2 == 6 | n1 <- u, odd n1, n2 <- u, odd n2]

-- 7. There is a number that is at least as large as every number
--    (i.e., according to >=)
-- A: 
prob7 = or[and [n1 >= n2 | n2 <- u]| n1 <-u]

-- 8. For every number, there is a different number such that there are no
--    numbers between these two.
-- A: 
prob8 = and[or [not (or [n1 < n2 && n2 < n3 | n2 <- u]) | n3 <- u, n3 /= n1] | n1 <- u] 