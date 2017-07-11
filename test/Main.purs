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
import IndexedDb.Store (Unique)
import IndexedDb.Transaction (Read, Write, VersionChangeTx)
import Test.Spec (SpecEffects, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Mocha (MOCHA, runMocha)

codec ∷ JsonCodec { id ∷ Int, value ∷ String }
codec = JA.object "Test record" $ JA.record
  # JA.recordProp (SProxy ∷ SProxy "id") JA.int
  # JA.recordProp (SProxy ∷ SProxy "value") JA.string

testStore ∷ Store Int { value ∷ Unique } { id ∷ Int, value ∷ String }
testStore = I.mkStore "test_store" (SProxy ∷ SProxy "id") codec

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
        I.add testStore {id: 1, value: "Hello, world"}
        I.get testStore 1
      (_.id <$> res) `shouldEqual` (Just 1) 
    it "returns Nothing when item is not found" do
      res ← liftReq $ runTx do
        I.get testStore 2
      (unit <$ res) `shouldEqual` Nothing
    it "cannot find a record that is deleted in the same tx" do
      res ← liftReq $ runTx do
        I.add testStore {id: 1, value: "Test value"}
        I.delete testStore 1
        I.get testStore 1
      (unit <$ res) `shouldEqual` Nothing
    it "updates a record field when passed to put as different" do
      res ← liftReq $ runTx do
        I.add testStore {id: 2, value: "Boom"}
        I.put testStore {id: 2, value: "Changed!"}
        I.get testStore 2
      (_.value <$> res) `shouldEqual` (Just "Changed!")

      --index testStore (SProxy ∷ SProxy "value")
