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


const StoriesDetails = (props) => {
    const [storyData, setStoryData] = useState(undefined);
    const [loading, setLoading] = useState(true);
    const [cardTabComponent, setCardTabComponent] = useState(undefined);
    const [error, setError] = useState(false);

    let { id } = useParams();

    useEffect(() => {
        const fetchData = async () => {
            try {
                const { data } = await axios.get(`http://localhost:4000/api/stories/${id}`);
                if (!data) throw "Couldnt get story details from API";
                setStoryData(data);
                setError(false)
                setLoading(false);
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
                let originalIssueUrl = 'https://www.marvel.com/stories';
                if (storyData.originalIssue) {

                    let comicID = storyData.originalIssue.resourceURI.substr(storyData.originalIssue.resourceURI.lastIndexOf('/'));
                    originalIssueUrl = "http://localhost:3000/comics" + comicID;
                }
                let description = storyData.description || 'N/A';
                let modified = (storyData.modified) ? new Date(storyData.modified).toLocaleString() : 'N/A';
                setCardTabComponent(
                    <>
                        <ListGroup.Item><p className='detailsBold'>Name </p>{storyData.title}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>ID </p>{storyData.id}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>Description </p>{description}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>Last Modified </p>{modified}</ListGroup.Item>
                        <ListGroup.Item>
                            <p className='detailsBold'>Marvel.com Official Resources </p>
                            <Card.Link href={originalIssueUrl}><Button variant="danger">Original Issue</Button></Card.Link>
                        </ListGroup.Item>
                    </>
                )
                return;
            case "characters":
                if (storyData.characters)
                    if (storyData.characters.items)
                        setCardTabComponent(
                            <>
                                {storyData.characters.items.map((character) => {
                                    let characterID = character.resourceURI.substr(character.resourceURI.lastIndexOf('/'));
                                    return (
                                        <ListGroup.Item key={characterID}>
                                            <Card.Link href={`http://localhost:3000/characters${characterID}`}>{character.name}</Card.Link>
                                        </ListGroup.Item>
                                    )
                                })}
                            </>
                        );
                return;
            case "comics":
                if (storyData.comics)
                    if (storyData.comics.items)
                        setCardTabComponent(
                            <>
                                {storyData.comics.items.map((comic) => {
                                    let comicID = comic.resourceURI.substr(comic.resourceURI.lastIndexOf('/'));
                                    return (
                                        <ListGroup.Item key={comicID}>
                                            <Card.Link href={`http://localhost:3000/stories${comicID}`}>{comic.name}</Card.Link>
                                        </ListGroup.Item>
                                    )
                                })}
                            </>
                        );
                return;
            default:
                return;
        };
    }

    if (error) return <Error />

    if (!cardTabComponent) handleCardNavigation("details");

    if (loading) return <h1 className="pageTitle">Loading....</h1>
    else {
        let thumbnail = (storyData.thumbnail) ? `${storyData.thumbnail.path}.${storyData.thumbnail.extension}` : "https://i.annihil.us/u/prod/marvel/i/mg/b/40/image_not_available.jpg";
        return (
            <Card className="detailsCard">
                <Card.Header><h1>{storyData.title}</h1></Card.Header>
                <Card.Img alt={storyData.title} src={thumbnail} />
                <Card.Body>
                    <ListGroup className='underlineLinks'>
                        <ListGroup.Item className='bg-danger'>
                            <Navbar className="normalLinks" bg="danger" variant="dark">
                                <Container className='justify-content-center'>
                                    <Nav defaultActiveKey="details" onSelect={handleCardNavigation}>
                                        <Nav.Item>
                                            <Nav.Link eventKey="details">Details</Nav.Link>
                                        </Nav.Item>
                                        <Nav.Item>
                                            <Nav.Link eventKey="characters">Characters</Nav.Link>
                                        </Nav.Item>
                                        <Nav.Item>
                                            <Nav.Link eventKey="comics">Comics</Nav.Link>
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

export default StoriesDetails;