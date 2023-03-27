const mongoCollections = require("../config/mongoCollections");
const users = mongoCollections.users;
const sweets = mongoCollections.sweets;
const { ObjectId } = require("mongodb");
const validate = require("../validation");

let createSweet = async (sweetText, sweetMood, userThatPosted) => {
  await validate.checkString(sweetText);
  await validate.checkSweetMood(sweetMood);
  await validate.checkUsername(userThatPosted);

  const sweetsCollection = await sweets();
  const newSweet = {
    sweetText: sweetText,
    sweetMood: sweetMood,
  };
};

let getSweetsByPage = async (n) => {
  await validate.checkPageNumber(n);
  const sweetsCollection = await sweets();
  const sweetsPage = await sweetsCollection.find(
    {},
    { sort: { _id: -1 }, skip: (n - 1) * 50, limit: 50 }
  );
  return sweetsPage;
};

let getSweetById = async (id) => {
  await validate.checkID(id);
  id = id.trim();
};
