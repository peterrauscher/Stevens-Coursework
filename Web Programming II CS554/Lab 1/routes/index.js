const sweetsRoutes = require("./sweets");

const constructorMethod = (app) => {
  app.use("/sweets", sweetsRoutes);

  app.use("*", (req, res) => {
    res.status(404).json({ error: "Not found" });
  });
};

module.exports = constructorMethod;
