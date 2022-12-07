let checkId = async (id) => {
  if (!id || typeof id !== "string" || !id.trim())
    throw "Invalid id, must be non-empty and not only spaces";
  return true;
};

let checkString = async (s) => {
  if (!s || typeof s !== "string" || !s.trim())
    throw "Invalid string, must be non-empty and not only spaces";
  return true;
};

module.exports = {
  checkId,
};
