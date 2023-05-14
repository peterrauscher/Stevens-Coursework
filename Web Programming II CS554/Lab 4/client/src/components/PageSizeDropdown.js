import React from 'react';
import Dropdown from 'react-bootstrap/Dropdown';

const PageSizeDropdown = (props) => {
    const handleChange = (e) => {
        props.pageSizeHandler(e);
    }
    return (
        <Dropdown onSelect={handleChange} value={props.pageSizeValue}>
            <Dropdown.Toggle variant="danger">
                Page Size
            </Dropdown.Toggle>
            <Dropdown.Menu>
                <Dropdown.Item eventKey="10">10</Dropdown.Item>
                <Dropdown.Item eventKey="20">20</Dropdown.Item>
                <Dropdown.Item eventKey="25">25</Dropdown.Item>
                <Dropdown.Item eventKey="50">50</Dropdown.Item>
                <Dropdown.Item eventKey="75">75</Dropdown.Item>
                <Dropdown.Item eventKey="100">100</Dropdown.Item>
            </Dropdown.Menu>
        </Dropdown >
    )
}

export default PageSizeDropdown;