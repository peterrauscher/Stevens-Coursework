import React, { useState, useEffect } from 'react';
import { useParams } from "react-router-dom";
import axios from 'axios';
import Card from 'react-bootstrap/Card';
import Button from 'react-bootstrap/Button';
import ListGroup from 'react-bootstrap/ListGroup';
import Nav from 'react-bootstrap/Nav';
import Navbar from 'react-bootstrap/Navbar';
import Container from 'react-bootstrap/Container';
import Error from './Error';

const CharacterDetails = (props) => {
    const [characterData, setCharacterData] = useState(undefined);
    const [loading, setLoading] = useState(true);
    const [cardTabComponent, setCardTabComponent] = useState(undefined);
    const [error, setError] = useState(false);
    let { id } = useParams();

    useEffect(() => {
        const fetchData = async () => {
            try {
                const { data } = await axios.get(`http://localhost:4000/api/characters/${id}`);
                if (!data) throw "Couldnt get character details from API";
                setCharacterData(data);
                setLoading(false);
                setError(false);
            } catch (e) {
                setError(true);
                console.log(e);
            }
        }
        fetchData();
    }, [id]);

    const handleCardNavigation = async (eventKey) => {
        switch (eventKey) {
            case "details":
                let marvelWiki = 'https://www.marvel.com/characters';
                if (characterData.urls)
                    if (characterData.urls[0])
                        if (characterData.urls[0].type === 'detail')
                            if (characterData.urls[0].url)
                                marvelWiki = characterData.urls[0].url;
                let comicsUrl = 'https://www.marvel.com/comics/characters';
                if (characterData.urls)
                    if (characterData.urls[2])
                        if (characterData.urls[2].type === 'comiclink')
                            if (characterData.urls[2].url)
                                comicsUrl = characterData.urls[2].url;
                setCardTabComponent(
                    <>
                        <ListGroup.Item><p className='detailsBold'>Name: </p>{characterData.name}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>ID: </p>{characterData.id}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>Description: </p>{characterData.description}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>Last Modified: </p>{new Date(characterData.modified).toLocaleString()}</ListGroup.Item>
                        <ListGroup.Item>
                            <p className='detailsBold'>Marvel.com Official Resources: </p>
                            <Card.Link href={marvelWiki}><Button variant="danger">Wiki</Button></Card.Link>
                            <Card.Link href={comicsUrl}><Button variant="danger">Comic Catalog</Button></Card.Link>
                        </ListGroup.Item>
                    </>
                )
                break;
            case "comics":
                if (characterData.comics)
                    if (characterData.comics.items)
                        setCardTabComponent(
                            <>
                                {characterData.comics.items.map((comic) => (
                                    <ListGroup.Item key={comic.id}>
                                        <Card.Link href={`http://localhost:3000/comics${comic.resourceURI.substr(comic.resourceURI.lastIndexOf('/'))}`}>{comic.name}</Card.Link>
                                    </ListGroup.Item>
                                ))}
                            </>
                        );
                return;
                break;
            case "stories":
                if (characterData.stories)
                    if (characterData.stories.items)
                        setCardTabComponent(
                            <>
                                {characterData.stories.items.map((story) => (
                                    <ListGroup.Item key={story.id}>
                                        <Card.Link href={`http://localhost:3000/stories${story.resourceURI.substr(story.resourceURI.lastIndexOf('/'))}`}>{story.name}</Card.Link>
                                    </ListGroup.Item>
                                ))}
                            </>
                        );
                return;
                break;
            default:
                return;
        };
    }

    if (error) return <Error />

    if (!cardTabComponent) handleCardNavigation("details");

    if (loading) return <h1 className="pageTitle">Loading....</h1>
    else {
        let thumbnail = (characterData.thumbnail) ? `${characterData.thumbnail.path}.${characterData.thumbnail.extension}` : "https://i.annihil.us/u/prod/marvel/i/mg/b/40/image_not_available.jpg";
        return (
            <Card className="detailsCard">
                <Card.Header><h1>{characterData.name}</h1></Card.Header>
                <Card.Img src={thumbnail} alt={characterData.name} />
                <Card.Body>
                    <ListGroup>
                        <ListGroup.Item className='bg-danger'>
                            <Navbar bg="danger" variant="dark">
                                <Container className='justify-content-center'>
                                    <Nav defaultActiveKey="details" onSelect={handleCardNavigation}>
                                        <Nav.Item>
                                            <Nav.Link eventKey="details">Details</Nav.Link>
                                        </Nav.Item>
                                        <Nav.Item>
                                            <Nav.Link eventKey="comics">Comics</Nav.Link>
                                        </Nav.Item>
                                        <Nav.Item>
                                            <Nav.Link eventKey="stories">Stories</Nav.Link>
                                        </Nav.Item>
                                    </Nav>
                                </Container>
                            </Navbar>
                        </ListGroup.Item>
                        {cardTabComponent}
                    </ListGroup>
                </Card.Body>
            </Card >
        )
    }
}

export default CharacterDetails;