module Test.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import DOM.Exception (message)
import Data.Array (length)
import Data.Codec as Codec
import Data.Either (either)
import Data.Foreign (readInt, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import IndexedDb (Database(Database), IDB, Request, Store, Transaction, Unique, NonUnique, Read, Write, VersionChangeTx, Version(Version), VersionMigration(VersionMigration))
import IndexedDb as I
import IndexedDb.KeyRange as KeyRange
import IndexedDb.Store (ForeignCodec)
import Test.Spec (SpecEffects, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (MOCHA, runMocha)

codec :: ForeignCodec { id :: Int
                     , slug :: String
                     , artist :: String
                     , album :: String
                     , year :: Int
                     }
codec = Codec.basicCodec dec toForeign
  where
  dec value = do
    id <- value ! "id" >>= readInt
    slug <- value ! "slug" >>= readString
    artist <- value ! "artist" >>= readString
    album <- value ! "album" >>= readString
    year <- value ! "year" >>= readInt
    pure { id, slug, artist, album, year }

testStore :: Store Int -- The type of the primary key
                  ( slug :: Unique, artist :: NonUnique ) -- Indices
                  ( id :: Int -- The record type to be stored
                  , slug :: String
                  , artist :: String
                  , album :: String
                  , year :: Int
                  )
testStore = I.mkStore "albums" (SProxy :: SProxy "id") codec

runTx :: forall eff a. Transaction (read :: Read, write :: Write) a -> Request (idb :: IDB | eff) a
runTx tx = do
  let migrations = singleton $ VersionMigration (Version 1) version1
  let testDb = (Database "test")
  I.deleteDatabase testDb
  idb <- I.open testDb (Version 1) migrations
  res <- I.runReadWriteTx idb [testStore] tx
  I.close idb
  pure res

  where
  version1 :: VersionChangeTx Unit
  version1 = void $ I.createObjectStore testStore

liftReq :: forall e a. Request e a -> Aff e a
liftReq req = do
  res <- runExceptT req
  either (throwError <<< error <<< message) pure res

main :: Eff (SpecEffects (mocha :: MOCHA, idb :: IDB)) Unit
main = runMocha do
  describe "Basic operations on an object store" do
    it "can get a record that was stored" do
      res <- liftReq $ runTx do
        I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
        I.get testStore 1
      (_.id <$> res) `shouldEqual` (Just 1)
    it "returns Nothing when item is not found" do
      res <- liftReq $ runTx do
        I.get testStore 2
      (unit <$ res) `shouldEqual` Nothing
    it "cannot find a record that is deleted in the same tx" do
      res <- liftReq $ runTx do
        I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
        I.delete testStore 1
        I.get testStore 1
      (unit <$ res) `shouldEqual` Nothing
    it "updates a record field when passed to put as different" do
      res <- liftReq $ runTx do
        I.add testStore {id: 2, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1996}
        I.put testStore {id: 2, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
        I.get testStore 2
      (_.year <$> res) `shouldEqual` (Just 1995)
    it "returns all records with getAll'" do
      res <- liftReq $ runTx do
        I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
        I.add testStore {id: 2, slug: "toto-mindfields", artist: "Toto", album: "Mindfields", year: 1999}
        I.getAll' testStore
      (_.id <$> res) `shouldEqual` [1, 2]
    it "can clear the entire object store" do
      res <- liftReq $ runTx do
        I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
        I.add testStore {id: 2, slug: "toto-mindfields", artist: "Toto", album: "Mindfields", year: 1999}
        I.clear testStore
        I.getAll' testStore
      length res `shouldEqual` 0

    describe "Indices" do
      it "can fetch a record using a unique index" do
        res <- liftReq $ runTx do
          I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
          I.index testStore (SProxy :: SProxy "slug") "toto-tambu"
        (_.id <$> res) `shouldEqual` (Just 1)

      it "returns Nothing when a record is not found using a unique index" do
        res <- liftReq $ runTx do
          I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
          I.index testStore (SProxy :: SProxy "slug") "Not found"
        (_.id <$> res) `shouldEqual` Nothing

      it "returns all records matching a non-unique index" do
        res <- liftReq $ runTx do
          I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
          I.add testStore {id: 2, slug: "toto-mindfields", artist: "Toto", album: "Mindfields", year: 1999}
          I.index testStore (SProxy :: SProxy "artist") "Toto"
        (_.year <$> res) `shouldEqual` [1995, 1999]

      it "returns all records greater than some value with getAll lowerBound" do
        res <- liftReq $ runTx do
          I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
          I.add testStore {id: 2, slug: "toto-mindfields", artist: "Toto", album: "Mindfields", year: 1999}
          I.add testStore {id: 3, slug: "toto-falling", artist: "Toto", album: "Falling In Between", year: 2006}
          I.add testStore {id: 4, slug: "toto-XIV", artist: "Toto", album: "Toto XIV", year: 2015}
          I.getAll testStore (KeyRange.lowerBound 2)
        (_.year <$> res) `shouldEqual` [2006, 2015]
