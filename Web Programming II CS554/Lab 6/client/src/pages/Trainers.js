import { React, useState, useEffect } from "react";
import { Alert, Col, Row } from "react-bootstrap";
import { useSelector } from "react-redux";
import CreateTrainer from "../components/CreateTrainer";
import Trainer from "../components/Trainer";

const Trainers = () => {
  const allTrainers = useSelector((state) => state.trainers.trainers);
  const [showAlert, setShowAlert] = useState(true);

  useEffect(() => {
    let alertRunningAnd = true;
    allTrainers.forEach((trainer) => {
      if (trainer.isActiveTrainer) alertRunningAnd = false;
    });
    setShowAlert(alertRunningAnd);
  }, [allTrainers]);

  return (
    <>
      <Alert variant="warning" hidden={!showAlert}>
        No trainer is selected! You won't be able to build a team until you
        select one.
      </Alert>
      <Row>
        <Col className="card-column">
          <CreateTrainer />
        </Col>
      </Row>
      <Row xs={1} lg={2}>
        {allTrainers.map((trainer) => {
          return <Trainer key={trainer.id} details={trainer} />;
        })}
      </Row>
    </>
  );
};

export default Trainers;
