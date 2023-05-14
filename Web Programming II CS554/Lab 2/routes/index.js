const characterRoutes = require("./characters");
const comicRoutes = require("./comics");
const storyRoutes = require("./stories");

const constructorMethod = (app) => {
  app.use("/api/characters", characterRoutes);
  app.use("/api/comics", comicRoutes);
  app.use("/api/stories", storyRoutes);

  app.use("*", (req, res) => {
    res.status(404).json({ error: "Resource not found" });
  });
};

module.exports = constructorMethod;
