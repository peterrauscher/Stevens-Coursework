const compression = require("compression");
const express = require("express");
const app = express();
const static = express.static(__dirname + "/public");

app.use("/public", static);
app.use(compression());

const configRoutes = require("./routes");
configRoutes(app);

app.listen(3000, () => {
  console.log("Server started at http://localhost:3000");
});
