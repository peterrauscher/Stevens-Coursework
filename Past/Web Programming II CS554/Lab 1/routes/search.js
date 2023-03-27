const express = require("express");
const router = express.Router();
const data = require("../data");
const searchData = data.search;

router.post("/", async (req, res) => {
  try {
    let searchTerm = req.body.showSearchTerm.toString();
    if (searchTerm.trim().length <= 0) {
      res.status(400).render("shows/error", {
        class: "error",
        message:
          "Search query must be non-empty and contain more than just whitespace",
      });
      return;
    }
    const searchInfo = await searchData.searchFromQuery(searchTerm);
    res.render("shows/search", {
      title: "Shows Found",
      searchTerm: searchTerm,
      shows: searchInfo,
    });
  } catch (e) {
    res.status(404).render("shows/error", {
      class: "show-not-found",
      message: "We're sorry, but no results were found for " + searchTerm + ".",
    });
  }
});

module.exports = router;
