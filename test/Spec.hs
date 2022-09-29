import Spec.Staking.Locked qualified as Locked
import Test.Tasty (defaultMain, testGroup)
import Prelude qualified as H

main :: H.IO ()
main = defaultMain (testGroup "Staking tests" [Locked.tests])
