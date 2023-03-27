import React from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faSearch } from "@fortawesome/free-solid-svg-icons";
import { Form, InputGroup } from "react-bootstrap";

const Search = (props) => {
  const handleChange = (e) => {
    props.searchValue(e.target.value);
  };
  return (
    <InputGroup className="search-bar">
      <InputGroup.Text>
        <FontAwesomeIcon icon={faSearch} />
      </InputGroup.Text>
      <Form.Control
        placeholder={props.placeholder}
        autoComplete="off"
        aria-label="Search Term"
        onChange={handleChange}
      />
    </InputGroup>
  );
};

export default Search;
