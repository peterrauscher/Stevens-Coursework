const express = require("express");
const cors = require("cors");
const PORT = 4000;

const app = express();
app.use(cors());

const main = async () => {
  const configRoutes = require("./routes");

  app.use(express.json());
  app.use(express.urlencoded({ extended: true }));

  configRoutes(app);

  app.listen(PORT, () => {
    console.log("Server is up!");
    console.log(`Your routes will be running on http://localhost:${PORT}`);
  });
};

main();
