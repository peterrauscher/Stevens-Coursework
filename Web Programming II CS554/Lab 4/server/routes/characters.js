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
const listLimitKey = "characterlistlimit";
const pagesKey = "characterpages";
const historyLength = 20;

client.connect().then(() => { });

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

router.get("/page/:pagenum", async (req, res) => {
  try {
    // Calculate the offset from page number and page size
    await validate.checkPageNum(req.params.pagenum);
    if (!req.query.page_size) req.query.page_size = 20;
    await validate.checkLimit(req.query.page_size);
    let pageNum = parseInt(req.params.pagenum);
    let pageSize = parseInt(req.query.page_size);
    let offset = (pageNum - 1) * pageSize;

    // Check if page size has changed since last request
    let lastPageSize = await client.get(listLimitKey);

    if (parseInt(lastPageSize) !== pageSize) {
      // If they have, update the page size and reset page cache
      console.log(
        `Page size has changed from ${lastPageSize} to ${pageSize}, clearing page cache`
      );
      await client.del(pagesKey);
      await client.set(listLimitKey, pageSize);
    }

    // Check if page exists in cache
    let pageExists = await client.hExists(pagesKey, pageNum.toString());
    if (!pageExists) {
      // If the page was NOT found in the cache, make an API request
      const response = await axios.get(
        auth.getAuthorizedUrlWithPagination(
          "https://gateway.marvel.com/v1/public/characters",
          offset,
          pageSize
        )
      );

      // Check for errors
      if (!response)
        return res
          .status(404)
          .json({ error: `There are no characters on page ${pageNum}` });
      let responseData = {
        maxPage: Math.ceil(parseInt(response.data.data.total) / pageSize),
        results: response.data.data.results
      }
      if (!responseData)
        return res
          .status(404)
          .json({ error: `There are no characters on page ${pageNum}` });
      if (response.data.code.toString() === "404")
        return res
          .status(404)
          .json({ error: `There are no characters on page ${pageNum}` });
      if (!(parseInt(response.data.data.count) > 0))
        return res
          .status(404)
          .json({ error: `There are no characters on page ${pageNum}` });

      // If everything was good, return the page, then flatten and cache it
      const flatStanley = JSON.stringify(flat(responseData));
      await client.hSet(pagesKey, pageNum.toString(), flatStanley);
      console.log(
        `Returned characters page ${pageNum} from API request and cached response`
      );
      return res.status(200).json(responseData);
    } else {
      // If the page WAS found in cache, return from there
      const cachedPage = await client.hGet(pagesKey, pageNum.toString());
      console.log(`Returned characters page ${pageNum} from cache`);
      let { maxPage, results } = unflatten(JSON.parse(cachedPage))
      return res
        .status(200)
        .json({ maxPage: maxPage, results: Object.values(results) });
    }
  } catch (e) {
    console.log(e);
    return res.status(400).json({ error: e });
  }
});

module.exports = router;
