module InbuildFunctions where


import DataStructures


inbuildPrint :: [Value] -> IO Value
inbuildPrint values = fmap (const VVoid) (mapM_ print values)

testPrint :: IO Value
testPrint = fmap (const VVoid) (print "TEST")
