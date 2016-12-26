
import MIPS_I
import Elf

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

main = do
    r <- runExceptT $ do
        img <- readElfFile "/media/tmpfs/testsuite/basic_tests/jumpr.elf"
        run img
    case r of
        Right _ -> print "Ok"
        Left s -> print $ "Fail: " ++ s
        

