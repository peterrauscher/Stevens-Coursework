import "./App.css";
import React, { useState, useEffect } from "react";
import { useQuery, useMutation } from "@apollo/client";
import Card from "react-bootstrap/Card";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";
import Button from "react-bootstrap/Button";
import Loading from "./Loading";
import Error from "./Error";
import queries from "../data/queries";
const regex = /(<([^>]+)>)/gi;

const PostsList = (props) => {
  const [pageNum, setPageNum] = useState(1);
  const [postsList, setPostsList] = useState([]);
  const { loading, error, data } = useQuery(props.query, {
    variables: { pageNum },
    fetchPolicy: "network-only",
  });
  const [updatePost] = useMutation(queries.UPDATE_IMAGE);
  const [deletePost] = useMutation(queries.DELETE_IMAGE);

  // On data changes
  useEffect(() => {
    if (data) {
      console.log(data);
      let iteratableData;
      switch (props.type) {
        case "Images":
          iteratableData = data.unsplashImages;
          break;
        case "Posts":
          iteratableData = data.userPostedImages;
          break;
        case "Binned":
          iteratableData = data.binnedImages;
          break;
        default:
          console.error("Unknown type: " + props.type);
      }
      if (iteratableData) {
        if (props.type === "Binned")
          setPostsList(
            postsList
              .concat(
                iteratableData.filter(
                  (image) => !postsList.find((post) => post.id === image.id)
                )
              )
              .filter((post) => post.binned)
          );
        else {
          setPostsList(
            postsList.concat(
              iteratableData.filter(
                (image) => !postsList.find((post) => post.id === image.id)
              )
            )
          );
        }
      }
    }
  }, [data]);

  const handleAddToBin = (e, postId) => {
    let postToUpdate = {};
    let newPostsList = postsList.map((post) => {
      if (post.id === postId) {
        postToUpdate = { ...post };
        postToUpdate.binned = !postToUpdate.binned;
        return postToUpdate;
      } else return post;
    });
    setPostsList(newPostsList);
    updatePost({
      variables: {
        id: postId,
        url: postToUpdate.url,
        posterName: postToUpdate.posterName,
        description: postToUpdate.description,
        userPosted: postToUpdate.userPosted,
        binned: postToUpdate.binned,
      },
    });
  };

  const handleDelete = (postId) => {
    deletePost({
      variables: {
        id: postId,
      },
    });
    setPostsList(postsList.filter((post) => post.id !== postId));
  };

  // Load or error
  if (!postsList && loading) return <Loading />;
  else if (error) return <Error message={error.message} />;

  // Build cards
  const posts = postsList.map((post) => {
    const deleteButton =
      props.type === "Posts" ? (
        <Button
          className="cardButton"
          variant="danger"
          onClick={(e) => handleDelete(post.id)}
        >
          Delete Post
        </Button>
      ) : (
        false
      );
    return (
      <Col className="card-column" key={post.id}>
        <Card className="card-component">
          <Card.Img alt={post.description} variant="top" src={post.url} />
          <Card.Body>
            <Card.Title className="card-title">
              {post.description
                ? post.description.replace(regex, "").substring(0, 139)
                : "No description"}
            </Card.Title>
            <Card.Text className="card-text">
              An image by:{" "}
              {post.posterName
                ? post.posterName.replace(regex, "").substring(0, 139)
                : "N/A"}
            </Card.Text>
            <Button
              className="cardButton"
              variant="primary"
              onClick={(e) => handleAddToBin(e, post.id)}
            >
              {post.binned ? "Remove from Bin" : "Add to Bin"}
            </Button>
            {deleteButton && deleteButton}
          </Card.Body>
        </Card>
      </Col>
    );
  });

  // Show "Get More" button when on the / page
  const getMoreButton =
    props.type === "Images" ? (
      <Button
        className="getMoreBtn"
        variant="primary"
        onClick={() => setPageNum(pageNum + 1)}
      >
        Get More
      </Button>
    ) : (
      false
    );

  // Return the page!
  return (
    <div className="contentBody">
      <h2>{props.type}</h2>
      <Row className="card-row" xs={1}>
        {posts && posts}
      </Row>
      {getMoreButton && getMoreButton}
    </div>
  );
};

export default PostsList;
