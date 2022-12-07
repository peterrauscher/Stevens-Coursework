const express = require("express");
const app = express();

const main = async () => {
  const configRoutes = require("./routes");

  app.use(express.json());
  app.use(express.urlencoded({ extended: true }));

  configRoutes(app);

  app.listen(3000, () => {
    console.log("Server is up!");
    console.log("Your routes will be running on http://localhost:3000");
  });
};

main();
