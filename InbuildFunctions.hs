module InbuildFunctions where


import DataStructures


inbuildPrint :: [Value] -> IO Value
inbuildPrint values = fmap (const VVoid) (mapM_ print values)
