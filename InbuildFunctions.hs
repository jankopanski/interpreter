module InbuildFunctions where


import DataStructures


inbuildPrint :: [Value] -> IO Value
inbuildPrint values = fmap (const VVoid) (mapM_ print values)

intToStr :: Value -> Value
intToStr (VInt n) = VString $ show n

strToInt :: Value -> Value
strToInt (VString s) = VInt $ read s
