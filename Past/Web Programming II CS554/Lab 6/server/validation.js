module.exports = {
  checkPageNumber: async (n) => {
    if (n === null) throw `Invalid data: page number cannot be null`;
    if (isNaN(n)) throw `Invalid data: ${n} is not a number`;
    if (parseInt(n) < 0) throw `Invalid data: page number ${n} is not >= 0`;
    return true;
  },
  checkId: async (id) => {
    if (id === null) throw `Invalid data: id cannot be null`;
    if (isNaN(id)) throw `Invalid data: id ${id} is not a number`;
    if (parseInt(id) < 1)
      throw `Invalid data: id ${id} is not a positive integer`;
    return true;
  },
  checkSearchTerm: async (s) => {
    if (s === null || !s || s.trim() === "")
      throw `Invalid data: search cannot be empty`;
    return true;
  },
};
