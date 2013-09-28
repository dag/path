{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Applicative ((<$>), pure)
import Data.Monoid (Monoid(..), (<>))
import Data.Path
import Data.Proxy (Proxy(..))
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

instance Arbitrary (Path Directory File) where
    arbitrary = do
        d <- arbitrary
        f <- file <$> arbitrary
        e <- arbitrary
        return (d </> f </> e)

instance Arbitrary (Path File File) where
    arbitrary = mconcat <$> listOf (ext <$> arbitrary)

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
    identity a = show (a </> Null) == show a && show (Null </> a) == show a
    associative :: a </> b -> b </> c -> c </> d -> Bool
    associative a b c = show (a </> (b </> c)) == show ((a </> b) </> c)

monoid :: forall m. (Arbitrary m, Monoid m, Show m) => Proxy m -> Spec
monoid _ = describe "Monoid laws" $ do
    prop "identity" identity
    prop "associative" associative
  where
    identity :: m -> Bool
    identity a = show (a <> mempty) == show a && show (mempty <> a) == show a
    associative :: m -> m -> m -> Bool
    associative a b c = show (a <> (b <> c)) == show ((a <> b) <> c)

main :: IO ()
main = hspec $ do

    describe "Root </> Directory" $
        category (Proxy :: Proxy (Root </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Root </> File" $
        category (Proxy :: Proxy (Root </> Directory))
            (Proxy :: Proxy (Directory </> File))

    describe "Drive </> Directory" $
        category (Proxy :: Proxy (Drive </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Drive </> File" $
        category (Proxy :: Proxy (Drive </> Directory))
            (Proxy :: Proxy (Directory </> File))

    describe "Remote </> Directory" $
        category (Proxy :: Proxy (Remote </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Remote </> File" $
        category (Proxy :: Proxy (Remote </> Directory))
            (Proxy :: Proxy (Directory </> File))

    describe "Home </> Directory" $
        category (Proxy :: Proxy (Home </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Home </> File" $
        category (Proxy :: Proxy (Home </> Directory))
            (Proxy :: Proxy (Directory </> File))

    describe "Working </> Directory" $
        category (Proxy :: Proxy (Working </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
    describe "Working </> File" $
        category (Proxy :: Proxy (Working </> Directory))
            (Proxy :: Proxy (Directory </> File))

    describe "Directory </> Directory" $ do
        category (Proxy :: Proxy (Directory </> Directory))
            (Proxy :: Proxy (Directory </> Directory))
        monoid (Proxy :: Proxy (Directory </> Directory))
    describe "Directory </> File" $
        category (Proxy :: Proxy (Directory </> Directory))
            (Proxy :: Proxy (Directory </> File))

    describe "File </> File" $ do
        category (Proxy :: Proxy (File </> File))
            (Proxy :: Proxy (File </> File))
        monoid (Proxy :: Proxy (File </> File))
