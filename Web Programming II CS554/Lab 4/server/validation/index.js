let checkString = async (s) => {
  if (!s || typeof s !== "string" || !s.trim())
    throw "Invalid string, must be non-empty and not only spaces";
  return true;
};

let checkId = async (id) => {
  if (!id || isNaN(id) || !id.trim())
    throw "Invalid id, must be non-empty and a number";
  return true;
};

let checkPageNum = async (n) => {
  if (!n || isNaN(n)) throw "Invalid page number, must be a number";
  if (parseInt(n) <= 0) throw "Invalid page number, must be greater than zero";
  return true;
};

let checkLimit = async (n) => {
  if (!n || isNaN(n)) throw "Invalid page size, must be a number";
  if (parseInt(n) <= 0 || parseInt(n) > 100)
    throw "Invalid page number, must be greater than zero and no more than 100";
  return true;
};

module.exports = {
  checkString,
  checkId,
  checkPageNum,
  checkLimit,
};
