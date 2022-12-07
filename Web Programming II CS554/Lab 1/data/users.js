const mongoCollections = require("../config/mongoCollections");
const users = mongoCollections.users;
const { ObjectId } = require("mongodb");
const bcrypt = require("bcrypt");
const saltRounds = 12;
const validate = require("../validation");

let createUser = async (name, username, password) => {
  await validate.checkString(name);
  name = name.trim();
  await validate.checkUsername(username);
  username = username.trim();
  await validate.checkPassword(password);

  const hash = await bcrypt.hash(password, saltRounds);

  const usersCollection = await users();
  const newUser = {
    name: name,
    username: username,
    password: hash,
  };

  const user = await usersCollection.findOne({
    username: username,
  });
  if (user) throw "Username is already taken";

  const insertInfo = await usersCollection.insertOne(newUser);
  if (insertInfo.insertedCount === 0) throw "Could not add user to database";

  return {
    _id: insertInfo.insertedId.toString(),
    name: name,
    username: username,
  };
};

let authenticateUser = async (username, password) => {
  await validate.checkUsername(username);
  username = username.trim();
  await validate.checkPassword(password);

  const usersCollection = await users();
  const user = await usersCollection.findOne({ username: { $regex: new RegExp("^" + username + "$", "i") } });
  if (!user) throw "No user with that username was found";

  const passwordStatus = await bcrypt.compare(password, user.password);
  if (!passwordStatus) throw "Incorrect password";
  return {
    _id: user._id.toString(),
    name: user.name,
    username: user.username,
  };
};
