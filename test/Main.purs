module Test.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import DOM.Exception (message)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as JA
import Data.Either (either)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import IndexedDb (Database(Database), IDB, Request, Store, Transaction, Version(Version), VersionMigration(VersionMigration))
import IndexedDb as I
import IndexedDb.Index (NonUnique, Unique)
import IndexedDb.Transaction (Read, Write, VersionChangeTx)
import Test.Spec (SpecEffects, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (MOCHA, runMocha)

codec ∷ JsonCodec { id ∷ Int
                  , slug ∷ String
                  , artist ∷ String
                  , album ∷ String
                  , year ∷ Int
                  }
codec = JA.object "Albums" $ JA.record
  # JA.recordProp (SProxy ∷ SProxy "id") JA.int
  # JA.recordProp (SProxy ∷ SProxy "slug") JA.string
  # JA.recordProp (SProxy ∷ SProxy "artist") JA.string
  # JA.recordProp (SProxy ∷ SProxy "album") JA.string
  # JA.recordProp (SProxy ∷ SProxy "year") JA.int

testStore ∷ Store Int ( slug ∷ Unique, artist ∷ NonUnique )
                      ( id ∷ Int
                      , slug ∷ String
                      , artist ∷ String
                      , album ∷ String
                      , year ∷ Int
                      )
testStore = I.mkStore "albums" (SProxy ∷ SProxy "id") codec

runTx ∷ ∀ eff a. Transaction (read ∷ Read, write ∷ Write) a → Request (idb ∷ IDB | eff) a
runTx tx = do
  let migrations = singleton $ VersionMigration (Version 1) version1
  let testDb = (Database "test")
  I.deleteDatabase testDb
  idb ← I.open testDb (Version 1) migrations
  res ← I.runReadWriteTx idb [testStore] tx
  I.close idb
  pure res

  where
  version1 ∷ VersionChangeTx Unit
  version1 = void $ I.createObjectStore testStore

liftReq ∷ ∀ e a. Request e a → Aff e a
liftReq req = do
  res ← runExceptT req
  either (throwError <<< error <<< message) pure res

main :: Eff (SpecEffects (mocha :: MOCHA, idb ∷ IDB)) Unit
main = runMocha do
  describe "Basic operations on an object store" do
    it "can get a record that was stored" do
      res ← liftReq $ runTx do
        I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
        I.get testStore 1
      (_.id <$> res) `shouldEqual` (Just 1) 
    it "returns Nothing when item is not found" do
      res ← liftReq $ runTx do
        I.get testStore 2
      (unit <$ res) `shouldEqual` Nothing
    it "cannot find a record that is deleted in the same tx" do
      res ← liftReq $ runTx do
        I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
        I.delete testStore 1
        I.get testStore 1
      (unit <$ res) `shouldEqual` Nothing
    it "updates a record field when passed to put as different" do
      res ← liftReq $ runTx do
        I.add testStore {id: 2, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1996}
        I.put testStore {id: 2, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
        I.get testStore 2
      (_.year <$> res) `shouldEqual` (Just 1995)

    describe "Indices" do
      it "can fetch a record using a unique index" do
        res ← liftReq $ runTx do
          I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
          I.index testStore (SProxy ∷ SProxy "slug") "toto-tambu"
        (_.id <$> res) `shouldEqual` (Just 1)

      it "returns Nothing when a record is not found using a unique index" do
        res ← liftReq $ runTx do
          I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
          I.index testStore (SProxy ∷ SProxy "slug") "Not found"
        (_.id <$> res) `shouldEqual` Nothing

      it "returns all records matching a non-unique index" do
        res ← liftReq $ runTx do
          I.add testStore {id: 1, slug: "toto-tambu", artist: "Toto", album: "Tambu", year: 1995}
          I.add testStore {id: 2, slug: "toto-mindfields", artist: "Toto", album: "Mindfields", year: 1999}
          I.index testStore (SProxy ∷ SProxy "artist") "Toto"
        (_.year <$> res) `shouldEqual` [1995, 1999]

