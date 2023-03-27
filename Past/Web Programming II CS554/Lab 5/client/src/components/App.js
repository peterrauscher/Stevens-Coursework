import logo from "../logo.svg";
import "./App.css";
import { BrowserRouter as Router, Link, Routes, Route } from "react-router-dom";
import Images from "./Images";
import Binned from "./Binned";
import Posts from "./Posts";
import NewPost from "./NewPost";
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

function App() {
  return (
    <ApolloProvider client={client}>
      <Router>
        <div className="App">
          <header className="App-header">
            <img src={logo} className="App-logo" alt="logo" />
            <h1 className="App-heading">Binterest</h1>
            <div className="App-nav-container">
              <ul className="App-nav">
                <li>
                  <Link className="contentLink" to="/my-bin">
                    my bin
                  </Link>
                </li>
                <li>
                  <Link className="contentLink" to="/">
                    images
                  </Link>
                </li>
                <li>
                  <Link className="contentLink" to="/my-posts">
                    my posts
                  </Link>
                </li>
                <li>
                  <Link className="contentLink" to="/new-post">
                    new post
                  </Link>
                </li>
              </ul>
            </div>
          </header>
          <div className="App-body">
            <Routes>
              <Route path="/" element={<Images />} />
              <Route path="/new-post" element={<NewPost />} />
              <Route path="/my-bin" element={<Binned />} />
              <Route path="/my-posts" element={<Posts />} />
            </Routes>
          </div>
        </div>
      </Router>
    </ApolloProvider>
  );
}

export default App;
