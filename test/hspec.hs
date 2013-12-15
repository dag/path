{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Applicative ((<$>), pure)
import Data.Monoid (Monoid(..), (<>))
import Data.Proxy (Proxy(..))
import System.Path
import System.Path.Core (Path(Nil))
import System.PathName ()
import System.PathName.Internal
import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..), oneof, listOf)
import Test.QuickCheck.Instances ()

instance Arbitrary Name where
    arbitrary = oneof [ByteString <$> arbitrary, Text <$> arbitrary]

instance Arbitrary (Path Root Directory) where
    arbitrary = pure root

instance Arbitrary (Path Drive Directory) where
    arbitrary = drive <$> arbitrary

instance Arbitrary (Path Remote Directory) where
    arbitrary = host <$> arbitrary

instance Arbitrary (Path Home Directory) where
    arbitrary = pure home

instance Arbitrary (Path Working Directory) where
    arbitrary = pure cwd

instance Arbitrary (Path Directory Directory) where
    arbitrary = mconcat <$> listOf (dir <$> arbitrary)

instance Arbitrary (Path Directory Regular) where
    arbitrary = do
        d <- arbitrary
        f <- file <$> arbitrary
        e <- arbitrary
        return (d </> f </> e)

instance Arbitrary (Path Regular Regular) where
    arbitrary = mconcat <$> listOf (ext <$> arbitrary)

deriving instance Arbitrary PathName
deriving instance Arbitrary FileName

category :: forall a b c d.
    ( Arbitrary (a </> b)
    , Arbitrary (b </> c)
    , Arbitrary (c </> d)
    ) => Proxy (a </> b) -> Proxy (c </> d) -> Spec
category _ _ = describe "Category laws" $ do
    prop "identity" identity
    prop "associative" associative
  where
    identity :: a </> b -> Bool
    identity a = (a </> Nil) == a && (Nil </> a) == a
    associative :: a </> b -> b </> c -> c </> d -> Bool
    associative a b c = (a </> (b </> c)) == ((a </> b) </> c)

monoid :: forall m. (Arbitrary m, Monoid m, Show m, Eq m) => Proxy m -> Spec
monoid _ = describe "Monoid laws" $ do
    prop "identity" identity
    prop "associative" associative
  where
    identity :: m -> Bool
    identity a = (a <> mempty) == a && (mempty <> a) == a
    associative :: m -> m -> m -> Bool
    associative a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = hspec $ do

    describe "Root </> Directory" $
        category (Proxy :: Proxy (Root </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Root </> Regular" $
        category (Proxy :: Proxy (Root </> Directory))
            (Proxy :: Proxy (Directory </> Regular))

    describe "Drive </> Directory" $
        category (Proxy :: Proxy (Drive </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Drive </> Regular" $
        category (Proxy :: Proxy (Drive </> Directory))
            (Proxy :: Proxy (Directory </> Regular))

    describe "Remote </> Directory" $
        category (Proxy :: Proxy (Remote </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Remote </> Regular" $
        category (Proxy :: Proxy (Remote </> Directory))
            (Proxy :: Proxy (Directory </> Regular))

    describe "Home </> Directory" $
        category (Proxy :: Proxy (Home </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Home </> Regular" $
        category (Proxy :: Proxy (Home </> Directory))
            (Proxy :: Proxy (Directory </> Regular))

    describe "Working </> Directory" $
        category (Proxy :: Proxy (Working </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Working </> Regular" $
        category (Proxy :: Proxy (Working </> Directory))
            (Proxy :: Proxy (Directory </> Regular))

    describe "Directory </> Directory" $ do
        category (Proxy :: Proxy (Directory </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
        monoid (Proxy :: Proxy (Directory </> Directory))
    describe "Directory </> Regular" $
        category (Proxy :: Proxy (Directory </> Directory))
            (Proxy :: Proxy (Directory </> Regular))

    describe "Regular </> Regular" $ do
        category (Proxy :: Proxy (Regular </> Regular))
            (Proxy :: Proxy (Regular </> Regular))
        monoid (Proxy :: Proxy (Regular </> Regular))

    describe "PathName" $
        monoid (Proxy :: Proxy PathName)

    describe "FileName" $
        monoid (Proxy :: Proxy FileName)
