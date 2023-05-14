import React, { useState, useEffect } from "react";
import { Link } from "react-router-dom";
import axios from "axios";
import Card from "react-bootstrap/Card";
import Row from "react-bootstrap/Row";
import Col from 'react-bootstrap/Col';
import Error from "./Error";

const CharacterHistory = () => {
    const regex = /(<([^>]+)>)/gi;
    const [loading, setLoading] = useState(true);
    const [charactersData, setCharactersData] = useState(undefined);
    const [error, setError] = useState(false);
    let cardColumns = null;

    //On first page load
    useEffect(() => {
        async function fetchData() {
            try {
                const { data } = await axios.get(`http://localhost:4000/api/characters/history`);
                if (!data) throw "Failed to get character history!";
                setCharactersData(data);
                setLoading(false);
                setError(false);
            } catch (e) {
                setCharactersData(undefined);
                setLoading(false);
                setError(true);
                console.log(e);
            }
        }
        fetchData();
    }, []);

    if (error) return <Error />

    const buildCard = (character) => {
        return (
            <Col className="card-column" key={character.id}>
                <Card className="card-component">
                    <Link className="card-link" to={`/characters/${character.id}`}>
                        <Card.Img alt={character.name} variant='top' src={character.thumbnail.path + "." + character.thumbnail.extension} />
                        <Card.Body>
                            <Card.Title className="card-title">{character.name}</Card.Title>
                            <Card.Text className="card-text">{character.description
                                ? character.description.replace(regex, '').substring(0, 139) + '...'
                                : 'No Description'}</Card.Text>
                        </Card.Body>
                    </Link>
                </Card >
            </Col>
        )
    };

    cardColumns = charactersData && charactersData.map((character) => buildCard(character));

    if (loading) {
        return (
            <div>
                <h1>Loading....</h1>
            </div>
        )
    } else {
        return (
            <div>
                <h1 className="pageTitle">Character History</h1>
                <div className="contentBody">
                    <Row className="card-row" xs={1} md={3}>
                        {cardColumns}
                    </Row>
                </div >
            </div>
        );
    }
}

export default CharacterHistory;