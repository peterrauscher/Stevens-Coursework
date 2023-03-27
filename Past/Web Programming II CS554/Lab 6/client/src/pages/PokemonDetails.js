import { useEffect, useState } from "react";
import { useParams } from "react-router-dom";
import { useQuery } from "@apollo/client";
import TypesList from "../components/TypesList";
import Error from "../components/Error";
import Loading from "../components/Loading";
import queries from "../data/queries";
import { Alert, Button, ListGroup, Table, Card } from "react-bootstrap";
import MovesList from "../components/MovesList";
import AbilitiesList from "../components/AbilitiesList";
import { useDispatch } from "react-redux";
import { catchRelease } from "../redux/slices/trainersSlice";
import { useSelector } from "react-redux";

const roundOff = (num, places) => {
  const x = Math.pow(10, places);
  return Math.round(num * x) / x;
};

const PokemonDetails = () => {
  let { id } = useParams();
  const dispatch = useDispatch();
  const [activeTrainer, setActiveTrainer] = useState(undefined);
  const [showAlert, setShowAlert] = useState(true);
  const [details, setDetails] = useState(null);
  const allTrainers = useSelector((state) => state.trainers.trainers);
  const { loading, error, data } = useQuery(queries.GET_POKEMON_DETAILS, {
    variables: { pokemonId: parseInt(id) },
    fetchPolicy: "network-only",
  });

  useEffect(() => {
    if (data) setDetails(data.pokemonById);
  }, [data]);

  useEffect(() => {
    let alertRunningAnd = true;
    setActiveTrainer(
      allTrainers.find((trainer) => {
        if (trainer.isActiveTrainer) {
          alertRunningAnd = false;
          return true;
        }
      })
    );
    setShowAlert(alertRunningAnd);
  }, [allTrainers]);

  const handleCatchRelease = (pokemonId) => {
    dispatch(catchRelease(pokemonId));
  };

  if (error) return <Error message="Error 404: No Pokemon with that ID!" />;
  if (loading) return <Loading />;
  if (details) {
    return (
      <>
        <Alert variant="warning" hidden={!showAlert}>
          No trainer is selected! You won't be able to build a team until you
          select one.
        </Alert>
        <Card className="details" border="dark">
          <Card.Header>
            <Card.Title>{details.name.toString().toUpperCase()}</Card.Title>
          </Card.Header>
          <Card.Img
            variant="top"
            className="details-image"
            src={details.thumbnail}
            alt={`Artwork for ${details.name}`}
          />
          <Card.Body>
            <ListGroup>
              <ListGroup.Item>
                <Button
                  className={!activeTrainer ? "disabled" : ""}
                  variant={!activeTrainer ? "secondary" : "primary"}
                  onClick={() => handleCatchRelease(details.id)}
                >
                  {activeTrainer && activeTrainer.team.includes(details.id)
                    ? "Release!"
                    : "Catch!"}
                </Button>
              </ListGroup.Item>
              <ListGroup.Item>
                <strong>ID</strong>
                <div>{details.id}</div>
              </ListGroup.Item>
              <ListGroup.Item>
                <strong>Types</strong>
                <div>
                  <TypesList types={details.types} />
                </div>
              </ListGroup.Item>
              <ListGroup.Item>
                <strong>Height/Weight</strong>
                <div>
                  {details.height * 10}cm/
                  {roundOff(details.weight * 0.1, 2)}kg
                </div>
              </ListGroup.Item>
              <ListGroup.Item>
                <strong>Base Stats</strong>
                <Table striped bordered hover size="sm">
                  <thead>
                    <tr>
                      <th>HP</th>
                      <th>Att</th>
                      <th>Def</th>
                      <th>Sp. Att</th>
                      <th>Sp. Def</th>
                      <th>Spd</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>{details.hp}</td>
                      <td>{details.attack}</td>
                      <td>{details.defense}</td>
                      <td>{details.special_attack}</td>
                      <td>{details.special_defense}</td>
                      <td>{details.speed}</td>
                    </tr>
                  </tbody>
                </Table>
              </ListGroup.Item>
              <ListGroup.Item>
                <strong>Abilities</strong>
                <div>
                  <AbilitiesList abilities={details.abilities} />
                </div>
              </ListGroup.Item>
              <ListGroup.Item>
                <strong>Moves</strong>
                <div>
                  <MovesList moves={details.moves} />
                </div>
              </ListGroup.Item>
            </ListGroup>
          </Card.Body>
        </Card>
      </>
    );
  }
};

export default PokemonDetails;
