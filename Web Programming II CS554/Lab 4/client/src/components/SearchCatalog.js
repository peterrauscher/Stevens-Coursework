import React from 'react';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faSearch } from '@fortawesome/free-solid-svg-icons';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';

const SearchCatalog = (props) => {
	const handleChange = (e) => {
		props.searchValue(e.target.value);
	};
	return (
		<InputGroup className='inlineBar'>
			<InputGroup.Text><FontAwesomeIcon icon={faSearch} /></InputGroup.Text>
			<Form.Control placeholder={props.placeholder} autoComplete='off' aria-label="Search Term" onChange={handleChange} />
		</InputGroup>
	);
};

export default SearchCatalog;
