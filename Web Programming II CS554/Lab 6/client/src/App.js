import React from "react";
import "./css/App.css";
import { BrowserRouter as Router, Link, Routes, Route } from "react-router-dom";
import { Navbar, Nav, Container } from "react-bootstrap";
import Home from "./pages/Home";
import PokemonList from "./pages/PokemonList";
import Trainers from "./pages/Trainers";
import PokemonDetails from "./pages/PokemonDetails";
import Error from "./components/Error";
import {
  ApolloClient,
  HttpLink,
  InMemoryCache,
  ApolloProvider,
} from "@apollo/client";
const client = new ApolloClient({
  cache: new InMemoryCache(),
  link: new HttpLink({
    uri: "http://localhost:4000",
  }),
});

const navLink = (to, text) => {
  return (
    <Link className="nav-link" to={to}>
      {text}
    </Link>
  );
};

function App() {
  return (
    <ApolloProvider client={client}>
      <Router>
        <div className="App">
          <header className="App-header">
            <Navbar bg="dark" variant="dark">
              <Container>
                <Navbar.Brand>
                  <img
                    className="App-logo"
                    alt="Gotta catch 'em all!"
                    src="https://i.imgur.com/7tZ431f.png"
                    border="0"
                  />
                </Navbar.Brand>
                <Nav fill>
                  <Nav.Item>{navLink("/", "Home")}</Nav.Item>
                  <Nav.Item>{navLink("/trainers", "Trainers")}</Nav.Item>
                  <Nav.Item>{navLink("/pokemon/page/0", "Pokemon")}</Nav.Item>
                </Nav>
              </Container>
            </Navbar>
          </header>
          <div className="App-body">
            <Routes>
              <Route path="/" element={<Home />} />
              <Route path="/pokemon/page/:pagenum" element={<PokemonList />} />
              <Route path="/pokemon/:id" element={<PokemonDetails />} />
              <Route path="/trainers" element={<Trainers />} />
              <Route
                path="/*"
                element={<Error message={"Error 404: Page not found"} />}
              />
            </Routes>
          </div>
          <footer className="App-footer fixed-bottom bg-dark">
            <p>
              Peter Rauscher (10441546)
              <a className="nav-link" href="https://github.com/peterrauscher">
                Check me out on GitHub :D
              </a>
            </p>
          </footer>
        </div>
      </Router>
    </ApolloProvider>
  );
}

export default App;
