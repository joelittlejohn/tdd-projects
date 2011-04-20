import Test.HUnit

data Rule = Rule Char String
  deriving (Eq, Show)


applyRulesToChar :: [Rule] -> Char -> String
applyRulesToChar [] c = [c]
applyRulesToChar (Rule from to:rs) c
  | c == from = to
  | otherwise = applyRulesToChar rs c
  
applyRules :: [Rule] -> String -> String
applyRules rs = concatMap (applyRulesToChar rs)

iterateRules :: Int -> [Rule] -> String -> String
iterateRules n rs s
    | n < 0 = error "iterateRules: cannot apply rule a negative number of times"
    | n == 0 = s
    | otherwise = iterateRules (n-1) rs (applyRules rs s)


-- Test cases

tests = TestList
    [ TestLabel "test1" (TestCase (assertEqual "A, Rules=[], 1 iteration = A" "A" (applyRules [] "A")))
    , TestLabel "test2" (TestCase (assertEqual "A, Rules=[A->A], 1 iteration = A" "A" (applyRules [Rule 'A' "A"] "A")))
    , TestLabel "test3" (TestCase (assertEqual "A, Rules=[A->AA], 1 iteration = A" "AA" (applyRules [Rule 'A' "AA"] "A")))
    , TestLabel "test4" (TestCase (assertEqual "A, Rules=[A->B], 1 iteration = B" "B" (applyRules [Rule 'A' "B"] "A")))
    , TestLabel "test5" (TestCase (assertEqual "A, Rules=[A->ABCD], 1 iteration = ABCD" "ABCD" (applyRules [Rule 'A' "ABCD"] "A")))
    , TestLabel "test6" (TestCase (assertEqual "A, Rules=[], 2 iteration = A" "A" (iterateRules 2 [] "A")))
    , TestLabel "test7" (TestCase (assertEqual "A, Rules=[A->A], 2 iteration = A" "A" (iterateRules 2 [Rule 'A' "A"] "A")))
    , TestLabel "test8" (TestCase (assertEqual "A, Rules=[A->AA], 2 iteration = AAAA" "AAAA" (iterateRules 2 [Rule 'A' "AA"] "A")))
    , TestLabel "test9" (TestCase (assertEqual "A, Rules=[A->B], 1 iteration = B" "B" (iterateRules 2 [Rule 'A' "B"] "A")))
    , TestLabel "test10" (TestCase (assertEqual "A, Rules=[A->ABCD], 2 iteration = ABCDBCD" "ABCDBCD" (iterateRules 2 [Rule 'A' "ABCD"] "A")))
    , TestLabel "test11" (TestCase (assertEqual "A, Rules=[A->AB, B->C], 2 iterations = ABC" "ABC" (iterateRules 2 [Rule 'A' "AB", Rule 'B' "C"] "A")))
    , TestLabel "test12" (TestCase (assertEqual "A, Rules=[A->B, B->AB], 1 iteration = B" "B" (iterateRules 1 [Rule 'A' "B", Rule 'B' "AB"] "A")))
    , TestLabel "test13" (TestCase (assertEqual "A, Rules=[A->B, B->AB], 2 iterations = AB" "AB" (iterateRules 2 [Rule 'A' "B", Rule 'B' "AB"] "A")))
    , TestLabel "test14" (TestCase (assertEqual "A, Rules=[A->B, B->AB], 3 iterations = BAB" "BAB" (iterateRules 3 [Rule 'A' "B", Rule 'B' "AB"] "A")))
    , TestLabel "test15" (TestCase (assertEqual "A, Rules=[A->B, B->AB], 4 iterations = ABBAB" "ABBAB" (iterateRules 4 [Rule 'A' "B", Rule 'B' "AB"] "A")))
    , TestLabel "test16" (TestCase (assertEqual "A, Rules=[A->B, B->AB], 5 iterations = BABABBAB" "BABABBAB" (iterateRules 5 [Rule 'A' "B", Rule 'B' "AB"] "A")))
    , TestLabel "test17" (TestCase (assertEqual "A, Rules=[A->B, B->AB], 6 iterations = ABBABBABABBAB" "ABBABBABABBAB" (iterateRules 6 [Rule 'A' "B", Rule 'B' "AB"] "A")))
    ]

main = runTestTT tests

testShow = showIterations 10 [Rule 'A' "B", Rule 'B' "AB"] "A"

kochRules = [ Rule 'F' "F+F-F-F+F" ]
kochToLogo = [ Rule 'F' "fd 10 ", Rule '+' "lt 90 ", Rule '-' "rt 90 " ]

kochN n = applyRules kochToLogo $ iterateRules n kochRules "F" 

dragonRules = [Rule 'X' "X+YF", Rule 'Y' "FX-Y"]
dragonToLogo = [ Rule 'F' "fd 10 ", Rule '-' "lt 90 ", Rule '+' "rt 90 ", Rule 'X' "", Rule 'Y' ""]

dragonN n = applyRules dragonToLogo $ iterateRules n dragonRules "FX"



repeatRules :: [Rule] -> String -> [String]
repeatRules rs s = s : repeatRules rs (applyRules rs s)

showIterations n rs i = putStrLn $ unlines $ take n $ repeatRules rs i

