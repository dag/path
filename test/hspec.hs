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
    arbitrary = pure $ path root

instance Arbitrary (Path Drive Directory) where
    arbitrary = path . drive <$> arbitrary

instance Arbitrary (Path Remote Directory) where
    arbitrary = path . host <$> arbitrary

instance Arbitrary (Path Home Directory) where
    arbitrary = pure $ path home

instance Arbitrary (Path Working Directory) where
    arbitrary = pure $ path cwd

instance Arbitrary (Path Directory Directory) where
    arbitrary = mconcat <$> listOf (path . dir <$> arbitrary)

instance Arbitrary (Path Directory File) where
    arbitrary = do
        d <- arbitrary
        f <- path . file <$> arbitrary
        e <- arbitrary
        return (d </> f </> e)

instance Arbitrary (Path File File) where
    arbitrary = mconcat <$> listOf (path . ext <$> arbitrary)

deriving instance Arbitrary PathName
deriving instance Arbitrary FileName

category :: forall a b c d.
    ( Arbitrary (Path a b)
    , Arbitrary (Path b c)
    , Arbitrary (Path c d)
    ) => Proxy (Path a b) -> Proxy (Path c d) -> Spec
category _ _ = describe "Category laws" $ do
    prop "identity" identity
    prop "associative" associative
  where
    identity :: Path a b -> Bool
    identity a = (a </> Nil) == a && (Nil </> a) == a
    associative :: Path a b -> Path b c -> Path c d -> Bool
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

    describe "Path Root Directory" $
        category (Proxy :: Proxy (Path Root Directory))
            (Proxy :: Proxy (Path Directory Directory))
    describe "Path Root File" $
        category (Proxy :: Proxy (Path Root Directory))
            (Proxy :: Proxy (Path Directory File))

    describe "Path Drive Directory" $
        category (Proxy :: Proxy (Path Drive Directory))
            (Proxy :: Proxy (Path Directory Directory))
    describe "Path Drive File" $
        category (Proxy :: Proxy (Path Drive Directory))
            (Proxy :: Proxy (Path Directory File))

    describe "Path Remote Directory" $
        category (Proxy :: Proxy (Path Remote Directory))
            (Proxy :: Proxy (Path Directory Directory))
    describe "Path Remote File" $
        category (Proxy :: Proxy (Path Remote Directory))
            (Proxy :: Proxy (Path Directory File))

    describe "Path Home Directory" $
        category (Proxy :: Proxy (Path Home Directory))
            (Proxy :: Proxy (Path Directory Directory))
    describe "Path Home File" $
        category (Proxy :: Proxy (Path Home Directory))
            (Proxy :: Proxy (Path Directory File))

    describe "Path Working Directory" $
        category (Proxy :: Proxy (Path Working Directory))
            (Proxy :: Proxy (Path Directory Directory))
    describe "Path Working File" $
        category (Proxy :: Proxy (Path Working Directory))
            (Proxy :: Proxy (Path Directory File))

    describe "Path Directory Directory" $ do
        category (Proxy :: Proxy (Path Directory Directory))
            (Proxy :: Proxy (Path Directory Directory))
        monoid (Proxy :: Proxy (Path Directory Directory))
    describe "Path Directory File" $
        category (Proxy :: Proxy (Path Directory Directory))
            (Proxy :: Proxy (Path Directory File))

    describe "Path File File" $ do
        category (Proxy :: Proxy (Path File File))
            (Proxy :: Proxy (Path File File))
        monoid (Proxy :: Proxy (Path File File))

    describe "PathName" $
        monoid (Proxy :: Proxy PathName)

    describe "FileName" $
        monoid (Proxy :: Proxy FileName)
