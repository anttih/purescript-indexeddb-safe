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
