{-# LANGUAGE TemplateHaskell #-}
module ExampleSpec
  ( check
  )
where
import           Test.QuickCheck.All            ( quickCheckAll )
import           Test.QuickCheck                ( Property
                                                , (==>)
                                                )
prop_trivial_example :: Int -> Property
prop_trivial_example x = x > 2 ==> x > 2

return []
check :: IO Bool
check = $quickCheckAll
