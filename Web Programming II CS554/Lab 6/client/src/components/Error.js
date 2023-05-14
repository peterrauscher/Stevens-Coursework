import React from "react";

const Error = (props) => {
  return (
    <h1>
      {props.message ? props.message : "Error 400: Internal Server Error"}
    </h1>
  );
};

export default Error;
