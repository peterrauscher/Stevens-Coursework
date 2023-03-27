const express = require("express");
const app = express();
const session = require("express-session");

app.use(
  session({
    name: "AuthCookie",
    secret: "a87QdymZm*R&Qx",
    saveUninitialized: true,
    resave: false,
    cookie: { maxAge: 600000 },
  })
);

app.use(function (req, res, next) {
  res.locals.session = req.session;
  next();
});

const configRoutes = require("./routes");

app.use(express.json());
app.use(express.urlencoded({ extended: true }));

configRoutes(app);

app.listen(3000, () => {
  console.log("Server is up!");
  console.log("Your routes will be running on http://localhost:3000");
});
