{-# LANGUAGE Strict #-}
import MIPS_I
import Elf

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

main = do
    runExceptT $ do
        readElfFile "/media/tmpfs/testsuite/basic_tests/jumpr.elf"
        


