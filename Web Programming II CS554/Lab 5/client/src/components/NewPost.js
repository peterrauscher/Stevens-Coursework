import React, { useState } from "react";
import "./App.css";
import { useMutation } from "@apollo/client";
import queries from "../data/queries";
import validation from "../data/validation";
import Form from "react-bootstrap/Form";
import Button from "react-bootstrap/Button";
import Card from "react-bootstrap/Card";

const NewPost = () => {
  const [uploadImage] = useMutation(queries.UPLOAD_IMAGE);
  const [postDescription, setPostDescription] = useState("");
  const [postUrl, setPostUrl] = useState("");
  const [postAuthor, setPostAuthor] = useState("");

  const setData = (e) => {
    try {
      switch (e.target.id) {
        case "postDescription":
          setPostDescription(e.target.value);
          break;
        case "postUrl":
          setPostUrl(e.target.value);
          break;
        case "postAuthor":
          setPostAuthor(e.target.value);
          break;
        default:
          throw "setData was called from an invalid object";
      }
    } catch (e) {
      alert(`Error in setting state for post data: ${e}`);
      console.error(`Error in NewPost: ${e}`);
    }
  };

  const handleUpload = async () => {
    try {
      await validation.checkImageURL(postUrl);
      await validation.checkDescription(postDescription);
      await validation.checkPosterName(postAuthor);
      uploadImage({
        variables: {
          url: postUrl,
          description: postDescription,
          posterName: postAuthor,
        },
      });
    } catch (e) {
      alert(`Error uploading post: ${e}`);
      console.error(`Error in NewPost: ${e}`);
    }
    setPostDescription("");
    setPostUrl("");
    setPostAuthor("");
  };

  return (
    <Card className="newPostCard">
      <Card.Header>
        <Card.Title>Create a Post</Card.Title>
      </Card.Header>
      <Card.Body>
        <Form className="newPostCard">
          <Form.Group>
            <Form.Label>Description: </Form.Label>
            <Form.Control
              type="text"
              id="postDescription"
              onChange={setData}
              value={postDescription}
            />
          </Form.Group>
          <Form.Group>
            <Form.Label>Image URL: </Form.Label>
            <Form.Control
              type="text"
              id="postUrl"
              onChange={setData}
              value={postUrl}
            />
          </Form.Group>
          <Form.Group>
            <Form.Label>Author Name: </Form.Label>
            <Form.Control
              type="text"
              id="postAuthor"
              onChange={setData}
              value={postAuthor}
            />
          </Form.Group>
        </Form>
      </Card.Body>
      <Card.Footer>
        <Button variant="primary" onClick={handleUpload}>
          Submit
        </Button>
      </Card.Footer>
    </Card>
  );
};

export default NewPost;
