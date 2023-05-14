const checkDescription = async (d) => {
  if (typeof d !== "string") throw "description must be a string";
  return true;
};

const checkImageURL = async (u) => {
  if (!u || u.trim() === "") throw "URL cannot be empty";
  if (typeof u !== "string") throw "URL must be a string";
  return true;
};

const checkPosterName = async (a) => {
  if (typeof a !== "string") throw "posterName cannot be undefined";
  return true;
};

const checkID = async (id) => {
  if (!id || id.trim() === "") throw "ID Cannot be empty";
  if (typeof id !== "string") throw "ID must be a string";
  return true;
};

const checkBinned = async (b) => {
  if (typeof b !== "boolean") throw "binned must be a boolean";
  return true;
};

const checkUserPosted = async (b) => {
  if (typeof b !== "boolean") throw "userPosted must be a boolean";
  return true;
};

module.exports = {
  checkDescription,
  checkImageURL,
  checkPosterName,
  checkID,
  checkBinned,
  checkUserPosted,
};
