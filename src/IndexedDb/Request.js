// module IndexedDb.Request

var indexedDB = window.indexedDB
  || window.webkitIndexedDB
  || window.mozIndexedDB
  || window.OIndexedDB
  || window.msIndexedDB;

var IDBTransaction = window.IDBTransaction
  || window.webkitIDBTransaction
  || window.OIDBTransaction
  || window.msIDBTransaction;

exports.openImpl = function (dbName) {
  return function (version) {
    return function (upgradeneeded) {
      return function (error) {
        return function (success) {
          return function () {
            var openReq = indexedDB.open(dbName, version)
            openReq.onerror = function (event) {
              error(openReq.error)();
            };
            openReq.onsuccess = function (_) {
              success(openReq.result)();
            };
            openReq.onupgradeneeded = function (event) {
              upgradeneeded(event)(event.target.result)(event.target.transaction)();
            };
          };
        };
      }
    };
  };
};

exports.closeImpl = function (idb) {
  return function () {
    idb.close();
  };
};

exports.deleteDatabaseImpl = function (dbName) {
  return function (error) {
    return function (success) {
      return function () {
        try {
          var req = indexedDB.deleteDatabase(dbName);
          req.onerror = function () {
            error(req.error)();
          };
          req.onsuccess = function (e) {
            success({})();
          };
        } catch (e) {
          if (e instanceof DOMException) {
            return error(e)();
          } else {
            throw e;
          }
        }
      }
    }
  }
};

exports.transactionImpl = function (db) {
  return function (stores) {
    return function (flag) {
      return function (error) {
        return function (success) {
          return function () {
            try {
              var transaction = db.transaction(stores, flag)
            } catch (e) {
              if (e instanceof DOMException) {
                return error(e)();
              } else {
                throw e;
              }
            }
            // TODO Are these needed
            //transaction.oncomplete = function () {
            //  success({})();
            //};
            //transaction.onerror = function () {
            //  console.log('tx onerror', transaction.error, transaction)
            //  error(transaction.error)();
            //};
            //transaction.onabort = function () {
            //  console.log('tx onabort', transaction, transaction.error)
            //  error(transaction.error)();
            //};
            return success(transaction)();
          };
        };
      };
    };
  };
};

exports.objectStoreImpl = function (name) {
  return function (transaction) {
    return function () {
      return transaction.objectStore(name);
    }
  }
};

exports.addImpl = function (store) {
  return function (item) {
    return function (error) {
      return function (success) {
        return function () {
          try {
            var req = store.add(item)
            req.onsuccess = function (e) {
              success(req.result)();
            };
            req.onerror = function (e) {
              e.stopPropagation();
              error(req.error)();
            };
          } catch (e) {
            if (e instanceof DOMException) {
              return error(e)();
            } else {
              throw e;
            }
          }
        };
     };
    };
  }
};

exports.getImpl = function (nothing) {
  return function (just) {
    return function (store) {
      return function (key) {
        return function (error) {
          return function (success) {
            return function () {
              var req = store.get(key)
              req.onsuccess = function () {
                success(req.result === undefined ? nothing : just(req.result))();
              };
              req.onerror = function () {
                error(req.error)();
              };
            };
         };
        };
      }
    };
  };
};

exports.indexImpl = function (nothing) {
  return function (just) {
    return function (store) {
      return function (indexName) {
        return function (key) {
          return function (error) {
            return function (success) {
              return function () {
                var index = store.index(indexName)
                var req = index.openCursor(key)
                req.onsuccess = function () {
                  success(req.result == undefined ? nothing : just(req.result.value))();
                };
                req.onerror = function () {
                  error(req.error)();
                };
              };
           };
          };
        };
      };
    };
  };
};

exports.putImpl = function (store) {
  return function (item) {
    return function (error) {
      return function (success) {
        return function () {
          try {
            var req = store.put(item)
            req.onsuccess = function (e) {
              success(req.result)();
            };
            req.onerror = function (e) {
              e.stopPropagation();
              error(req.error)();
            };
          } catch (e) {
            if (e instanceof DOMException) {
              return error(e)();
            } else {
              throw e;
            }
          }
        };
     };
    };
  }
};

exports.createObjectStoreImpl = function (left) {
  return function (right) {
    return function (db) {
      return function (name) {
        return function (keyPath) {
          return function () {
            try {
              var store = db.createObjectStore(name, { keyPath: keyPath });
              return right(store);
            } catch (e) {
              if (e instanceof DOMException) {
                return left(e)();
              } else {
                throw e;
              }
            }
          };
        };
      };
    };
  };
};

exports.createIndexImpl = function (left) {
  return function (right) {
    return function (db) {
      return function (indexName) {
        return function (keyPath) {
          return function (unique) {
            return function () {
              try {
                var index = db.createIndex(indexName, keyPath, { unique: unique });
                return right(index);
              } catch (e) {
                if (e instanceof DOMException) {
                  return left(e)();
                } else {
                  throw e;
                }
              }
            };
          }
        };
      };
    };
  };
};

exports.deleteImpl = function (store) {
  return function (key) {
    return function (error) {
      return function (success) {
        return function () {
          try {
            var req = store.delete(key)
            req.onsuccess = function (e) {
              success(req.result)();
            };
            req.onerror = function (e) {
              e.stopPropagation();
              error(req.error)();
            };
          } catch (e) {
            if (e instanceof DOMException) {
              return error(e)();
            } else {
              throw e;
            }
          }
        };
     };
    };
  }
};

exports.abort = function (tx) {
  return function () {
    return tx.abort();
  };
};
