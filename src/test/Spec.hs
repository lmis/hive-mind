module Spec
  ( check
  )
where
import qualified ExampleSpec                    ( check )

check :: IO Bool
check = do
  putStrLn "---------------------------------- EXAMPLE ----------------------------------"
  ExampleSpec.check
