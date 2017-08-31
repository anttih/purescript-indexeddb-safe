// module IndexedDb.KeyRange

exports.lowerBound = function (a) {
  return IDBKeyRange.lowerBound(a, true);
};

exports.lowerBoundIncluding = function (a) {
  return IDBKeyRange.lowerBound(a, false);
};

exports.upperBound = function (a) {
  return IDBKeyRange.upperBound(a, true);
};

exports.upperBoundIncluding = function (a) {
  return IDBKeyRange.upperBound(a, false);
};

exports.bound = function (x) {
  return function (y) {
    return function (ex) {
      return function (ey) {
        return IDBKeyRange.bound(x, y, ex, ey);
      }
    }
  }
}
