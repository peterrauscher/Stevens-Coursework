import { Container, Row, Button, Col, Card } from "react-bootstrap";
import { Link } from "react-router-dom";
import { useDispatch } from "react-redux";
import {
  deleteTrainer,
  activateTrainer,
  deactivateTrainer,
} from "../redux/slices/trainersSlice";

const Trainer = (props) => {
  const dispatch = useDispatch();
  const handleActivateTrainer = (id) => {
    dispatch(activateTrainer(id));
  };
  const handleDeactivateTrainer = (id) => {
    dispatch(deactivateTrainer(id));
  };
  const handleDeleteTrainer = (id) => {
    dispatch(deleteTrainer(id));
  };

  const team = [...Array(6).keys()].map((i) => {
    let pokemonId = props.details.team[i];
    if (typeof pokemonId === "undefined") {
      return (
        <Col className="card-column">
          <Link to={`/pokemon/page/0`}>
            <Card.Img
              className={
                props.details.isActiveTrainer
                  ? "teammate-image-active"
                  : "teammate-image"
              }
              src="/empty-slot.png"
              alt={"Empty trainer team slot"}
            />
          </Link>
        </Col>
      );
    } else
      return (
        <Col className="card-column">
          <Link key={pokemonId} to={`/pokemon/${pokemonId}`}>
            <Card.Img
              className={
                props.details.isActiveTrainer
                  ? "teammate-image-active"
                  : "teammate-image"
              }
              src={`/official-artwork/${pokemonId}.png`}
              alt={"Pokemon with ID " + pokemonId}
            />
          </Link>
        </Col>
      );
  });

  return (
    <Col className="card-column">
      <Card
        bg={props.details.isActiveTrainer ? "primary" : "light"}
        text={props.details.isActiveTrainer ? "white" : "dark"}
      >
        <Card.Header>
          <Card.Title>{props.details.name}</Card.Title>
        </Card.Header>
        <Card.Body className="bg-light">
          <Container>
            <Row xs={2} md={3} xl={6}>
              {team}
            </Row>
          </Container>
        </Card.Body>
        <Card.Footer>
          <Button
            className="trainer-button"
            variant={props.details.isActiveTrainer ? "light" : "primary"}
            onClick={() => {
              props.details.isActiveTrainer
                ? handleDeactivateTrainer(props.details.id)
                : handleActivateTrainer(props.details.id);
            }}
          >
            {props.details.isActiveTrainer
              ? "Deselect Trainer"
              : "Select Trainer"}
          </Button>
          <Button
            className="trainer-button"
            variant={props.details.isActiveTrainer ? "light" : "primary"}
            onClick={() => {
              handleDeleteTrainer(props.details.id);
            }}
          >
            Delete Trainer
          </Button>
        </Card.Footer>
      </Card>
    </Col>
  );
};

export default Trainer;
