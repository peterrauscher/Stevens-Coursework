const app = require("express");
const http = require("http").createServer(app);
var io = require("socket.io")(http);

io.on("connection", (socket) => {
  console.log("new client connected", socket.id);

  socket.on("user_join", ({ name, room }) => {
    console.log("A user joined room " + room + " their name is " + name);
    socket.join(room);
    socket.to(room).emit("user_join", { name, room });
  });

  socket.on("message", ({ name, room, message }) => {
    console.log(name, room, message, socket.id);
    io.to(room).emit("message", { name, message });
  });

  socket.on("disconnect", () => {
    console.log("Disconnect Fired");
  });
});

http.listen(4000, () => {
  console.log(`listening on *:${4000}`);
});
