module Interpreter where

import LexMacchiato
import ParMacchiato
import SkelMacchiato
import PrintMacchiato
import AbsMacchiato

interpret :: Program-> IO ()
interpret (Program topdefs) = print topdefs
