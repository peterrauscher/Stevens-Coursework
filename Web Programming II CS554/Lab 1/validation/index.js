const { ObjectId } = require("mongodb");

let numError = "Invalid page number, must be a numerical nonnegative value";
let checkPageNumber = async (n) => {
  if (!n) {
    throw numError;
  }
  if (typeof n === "string") {
    if (n != parseInt(n)) {
      throw numError;
    }
  }
  n = parseInt(n);
  if (isNaN(n)) {
    throw numError;
  }
  if (n < 0) {
    throw numError;
  }
  return n;
};

let checkString = async (s) => {
  if (!s || typeof s !== "string" || !s.trim()) throw "Invalid string, must be non-empty and not only spaces";
  return true;
};

let checkUsername = async (user) => {
  if (!s || typeof s !== "string" || !s.trim()) throw "Invalid username, must be a non-empty string";
  if (s.includes(" ")) throw "Invalid username, cannot contain spaces";
  return true;
};

let validSweetMoods = [
  "Happy",
  "Sad",
  "Angry",
  "Excited",
  "Surprised",
  "Loved",
  "Blessed",
  "Greatful",
  "Blissful",
  "Silly",
  "Chill",
  "Motivated",
  "Emotional",
  "Annoyed",
  "Lucky",
  "Determined",
  "Bored",
  "Hungry",
  "Disappointed",
  "Worried",
];

let checkSweetMood = async (mood) => {
  if (!s || typeof s !== "string" || !s.trim()) throw "Invalid mood, must be non-empty and not only spaces";
  if (!validSweetMoods.includes(mood)) throw "Invalid mood, can only be one of the following values: " + validSweetMoods.join(", ");
  return true;
};
