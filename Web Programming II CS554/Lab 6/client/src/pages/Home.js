import { Alert } from "react-bootstrap";
import { Link } from "react-router-dom";

const Home = () => {
  return (
    <div className="jumbotron about-section">
      <h1 className="display-4">Gotta Catch 'Em All!</h1>
      <h2 className="display-6">
        A React/Redux/GraphQL Demo using the PokeAPI
      </h2>
      <p className="lead">
        Have fun and plan your team builds ahead of time for your next Pokemon
        playthrough, all in one place. Add trainers, "catch" any Pokemon you
        want from the Pokedex, and create an unstoppable team! Start your
        journey by adding some <Link to="/trainers">Trainers</Link>.
      </p>
      <hr className="my-4" />
      <Alert variant="danger">
        <p className="lead">
          This is a single-page application. Due to the nature of Redux stores,
          refreshing any page on this site will cause a complete loss of data
          including all trainers and their teams. This includes clicking
          refresh, the 'CTRL+R' shortcut, or manually editing the URL in the
          navigation bar.
        </p>
      </Alert>
    </div>
  );
};

export default Home;
