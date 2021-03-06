// module IndexedDb.Request.Internal

var indexedDB = window.indexedDB
  || window.webkitIndexedDB
  || window.mozIndexedDB
  || window.OIndexedDB
  || window.msIndexedDB;

var IDBTransaction = window.IDBTransaction
  || window.webkitIDBTransaction
  || window.OIDBTransaction
  || window.msIDBTransaction;

exports.open = function (dbName, version, upgradeneeded, error, success) {
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

exports.close = function (idb) {
  return function () {
    idb.close();
  };
};

exports.deleteDatabase = function (dbName, error, success) {
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

exports.transaction = function (db, stores, flag, error, success) {
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

exports.objectStore = function (name, transaction) {
  return transaction.objectStore(name);
};

exports.add = function (store, item, error, success) {
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

exports.clear = function (store, error, success) {
  try {
    var req = store.clear();
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
}

exports.get = function (nothing, just, store, key, error, success) {
  var req = store.get(key)
  req.onsuccess = function () {
    success(req.result === undefined ? nothing : just(req.result))();
  };
  req.onerror = function () {
    error(req.error)();
  };
};

exports.getAll = function (store, error, success) {
  var req = store.getAll();
  req.onsuccess = function () {
    success(req.result)();
  };
  req.onerror = function () {
    error(req.error)();
  };
};

exports.getAllByKey = function (store, key, error, success) {
  var req = store.getAll(key);
  req.onsuccess = function () {
    success(req.result)();
  };
  req.onerror = function () {
    error(req.error)();
  };
};

exports.index = function (nothing, just, store, indexName, key, error, success) {
  var index = store.index(indexName)
  var req = index.openCursor(key)
  req.onsuccess = function () {
    success(req.result == undefined ? nothing : just(req.result.value))();
  };
  req.onerror = function () {
    error(req.error)();
  };
};

exports.indexNonUnique = function (store, indexName, key, error, success) {
  var index = store.index(indexName)
  var req = index.openCursor(key)
  var items = [];
  req.onsuccess = function (event) {
    var cursor = event.target.result;
    if (cursor) {
      items.push(cursor.value);
      cursor.continue();
    } else {
      success(items)();
    }
  };
  req.onerror = function () {
    error(req.error)();
  };
};

exports.put = function (store, item, error, success) {
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

exports.createObjectStore = function (db, name, keyPath, error, success) {
  try {
    var store = db.createObjectStore(name, { keyPath: keyPath });
    return success(store)();
  } catch (e) {
    if (e instanceof DOMException) {
      return error(e)();
    } else {
      throw e;
    }
  }
};

exports.createIndex = function (db, indexName, keyPath, unique, error, success) {
  try {
    var index = db.createIndex(indexName, keyPath, { unique: unique });
    return success(index)();
  } catch (e) {
    if (e instanceof DOMException) {
      return error(e)();
    } else {
      throw e;
    }
  }
};

exports.deleteIndex = function (db, indexName, error, success) {
  try {
    var index = db.deleteIndex(indexName);
    return success();
  } catch (e) {
    if (e instanceof DOMException) {
      return error(e)();
    } else {
      throw e;
    }
  }
};

exports.delete = function (store, key, error, success) {
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

exports.abort = function (tx) {
  return function () {
    return tx.abort();
  };
};
