const express = require("express");
const router = express.Router();
const data = require("../data");
const showData = data.show;

router.get("/:id", async (req, res) => {
  try {
    let showInfo = await showData.getShowById(req.params.id);

    if (!showInfo.name) showInfo.name = "N/A";
    if (!showInfo.image.medium) showInfo.image.medium = "/public/not-found.png";
    if (!showInfo.language) showInfo.language = "N/A";
    if (!showInfo.genres) showInfo.genres = ["N/A"];
    if (!showInfo.rating.average) showInfo.rating.average = "N/A";
    if (!showInfo.network.name) showInfo.network.name = "N/A";
    if (!showInfo.summary) showInfo.summary = "N/A";

    res.render("shows/show", {
      title: showInfo.name,
      imageSrc: showInfo.image.medium,
      language: showInfo.language,
      genres: showInfo.genres,
      ratingAverage: showInfo.rating.average,
      networkName: showInfo.network.name,
      summary: showInfo.summary,
    });
  } catch (e) {
    res
      .status(404)
      .render("shows/error", { class: "error-not-found", message: e });
  }
});

module.exports = router;
