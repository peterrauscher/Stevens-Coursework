import React, { useState } from "react";
import { Alert, Button, Card, Form } from "react-bootstrap";
import { useDispatch } from "react-redux";
import { createTrainer } from "../redux/slices/trainersSlice";

const CreateTrainer = () => {
  const dispatch = useDispatch();
  const [formName, setFormName] = useState("");
  const [showAlert, setShowAlert] = useState(false);

  const handleChange = (e) => {
    setFormName(e.target.value);
    setShowAlert(false);
  };

  const handleButton = () => {
    if (formName === "") setShowAlert(true);
    else {
      dispatch(createTrainer(formName));
      setFormName("");
    }
  };

  return (
    <Card>
      <Card.Header>
        <Card.Title>Create Trainer</Card.Title>
      </Card.Header>
      <Card.Body>
        <Alert variant="danger" hidden={!showAlert}>
          You must enter a name!
        </Alert>
        <Form>
          <Form.Group>
            <Form.Label htmlFor="createTrainerInput">Trainer Name</Form.Label>
            <Form.Control
              type="text"
              id="createTrainerInput"
              placeholder="Enter name"
              onChange={(e) => handleChange(e)}
              value={formName}
            />
            <Form.Text className="text-muted">
              You can't change this later. To change a name a new trainer would
              need to be created and your team would be lost.
            </Form.Text>
          </Form.Group>
        </Form>
      </Card.Body>
      <Card.Footer>
        <Button variant="primary" onClick={handleButton}>
          Create
        </Button>
      </Card.Footer>
    </Card>
  );
};

export default CreateTrainer;
