const express = require("express");
const axios = require("axios");
const router = express.Router();
const redis = require("redis");
const client = redis.createClient();
const flat = require("flat");
const unflatten = flat.unflatten;
const auth = require("../auth");
const validate = require("../validation");
const historyKey = "characterhistory";
const cacheKey = "characters";
const historyLength = 20;

client.connect().then(() => {});

router.get("/:id", async (req, res) => {
  try {
    await validate.checkId(req.params.id);
    let id = req.params.id.toString();
    if (id === "history") {
      let exists = await client.exists(historyKey);
      if (!exists) return res.status(200).json([]);
      const mostRecentIds = await client.lRange(
        historyKey,
        0,
        historyLength - 1
      );
      let mostRecentCharacters = [];
      for (i in mostRecentIds) {
        const character = await client.hGet(cacheKey, mostRecentIds[i]);
        mostRecentCharacters[i] = unflatten(JSON.parse(character));
      }
      return res.status(200).json(mostRecentCharacters);
    } else {
      let exists = await client.hExists(cacheKey, id);
      if (!exists) {
        const response = await axios.get(
          auth.getAuthorizedUrl(
            "https://gateway.marvel.com/v1/public/characters/" + id
          )
        );
        if (!response)
          return res
            .status(404)
            .json({ error: "Character with id " + id + " not found" });
        let responseData = response.data.data.results[0];
        if (!responseData)
          return res
            .status(404)
            .json({ error: "Character with id " + id + " not found" });
        if (response.data.code.toString() === "404")
          return res
            .status(404)
            .json({ error: "Character with id " + id + " not found" });
        const flatStanley = JSON.stringify(flat(responseData));
        await client.hSet(cacheKey, id, flatStanley);
        await client.lPush(historyKey, id);
        return res.status(200).json(responseData);
      } else {
        const cachedResponse = await client.hGet(cacheKey, id);
        await client.lPush(historyKey, id);
        return res.status(200).json(unflatten(JSON.parse(cachedResponse)));
      }
    }
  } catch (e) {
    console.log(e);
    return res.status(400).json({ error: e });
  }
});

module.exports = router;
