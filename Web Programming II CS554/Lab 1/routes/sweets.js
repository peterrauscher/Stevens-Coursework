const express = require("express");
const router = express.Router();
const data = require("../data");
const sweetsData = data.sweets;
const usersData = data.users;
const validate = require("../validation");
const xss = require("xss");

router.get("/", async (req, res) => {
  try {
    let pageNumber;
    if (req.query.page === undefined) pageNumber = 1;
    else pageNumber = await validate.checkPageNumber(req.query.page);
    try {
      let sweetsPage = await sweetsData.getSweetsByPage(pageNumber);
      res.status(200).json(sweetsPage);
    } catch (e) {
      res.status(404).json({ error: e }); //`No more sweets to display!`
    }
  } catch (e) {
    res.status(400).json({ error: e }); //Bad input
  }
});

router.get("/:id", async (req, res) => {
  try {
    validate.checkID(req.params.id);
    let sweet = await sweetsData.getSweetById(req.params.id);
    res.json(sweet);
  } catch (e) {
    res.status(404).json({ error: e }); //`No sweet with id "${req.params.id}"`
  }
});

router.post("/", async (req, res) => {
  try {
    const newSweet = await sweetsData.createSweet();
    res.status(200).json(newSweet);
  } catch (e) {
    res.status(400).json({ error: e });
  }
});

router.patch("/:id", async (req, res) => {
  try {
    let updatedSweet = await sweetsData.updateSweet();
  } catch (e) {
    res.status(400).json({ error: e });
  }
});

router.post("/:id/replies", async (req, res) => {});

router.post("/signup", async (req, res) => {
  try {
    req.body.username = xss(req.body.username);
    await validate.checkUsername(req.body.username);
    req.body.password = xss(req.body.password);
    await validate.checkPassword(req.body.password);
    req.body.name = xss(req.body.name);
    await validate.checkString(req.body.name);

    const result = await usersData.createUser(
      req.body.name,
      req.body.username,
      req.body.password
    );
    if (result) {
    }
  } catch (e) {
    res.status(400).json({ error: e });
  }
});

module.exports = router;
