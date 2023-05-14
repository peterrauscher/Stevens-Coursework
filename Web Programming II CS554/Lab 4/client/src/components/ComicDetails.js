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


const ComicDetails = (props) => {
    const [comicData, setComicData] = useState(undefined);
    const [loading, setLoading] = useState(true);
    const [cardTabComponent, setCardTabComponent] = useState(undefined);
    const [error, setError] = useState(false);

    let { id } = useParams();

    useEffect(() => {
        const fetchData = async () => {
            try {
                const { data } = await axios.get(`http://localhost:4000/api/comics/${id}`);
                if (!data) throw "Couldnt get comic details from API";
                setComicData(data);
                setLoading(false);
            } catch (e) {
                console.log(e);
            }
        }
        fetchData();
    }, [id]);

    const handleCardNavigation = async (eventKey) => {
        switch (eventKey) {
            case "details":
                let marvelWiki = 'https://www.marvel.com/comics';
                if (comicData.urls)
                    if (comicData.urls[0])
                        if (comicData.urls[0].type === 'detail')
                            if (comicData.urls[0].url)
                                marvelWiki = comicData.urls[0].url;
                let purchaseURL = 'https://comicstore.marvel.com';
                if (comicData.urls)
                    if (comicData.urls[1])
                        if (comicData.urls[1].type === 'purchase')
                            if (comicData.urls[1].url)
                                purchaseURL = comicData.urls[1].url;
                let description = comicData.description || 'N/A';
                let issueNumber = parseInt(comicData.issueNumber) || 'N/A';
                let modified = (comicData.modified) ? new Date(comicData.modified).toLocaleString() : 'N/A';
                let pageCount = parseInt(comicData.pageCount) || 'N/A';
                let digitalID = comicData.digitalId || 'N/A';
                setCardTabComponent(
                    <>
                        <ListGroup.Item><p className='detailsBold'>Name </p>{comicData.title}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>ID </p>{comicData.id}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>Digital ID </p>{digitalID}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>Description </p>{description}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>Issue No. </p>{issueNumber}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>Page Count </p>{pageCount}</ListGroup.Item>
                        <ListGroup.Item><p className='detailsBold'>Last Modified </p>{modified}</ListGroup.Item>
                        <ListGroup.Item>
                            <p className='detailsBold'>Marvel.com Official Resources </p>
                            <Card.Link href={marvelWiki}><Button variant="danger">Wiki</Button></Card.Link>
                            <Card.Link href={purchaseURL}><Button variant="danger">Purchase</Button></Card.Link>
                        </ListGroup.Item>
                    </>
                )
                break;
            case "characters":
                if (comicData.characters)
                    if (comicData.characters.items)
                        setCardTabComponent(
                            <>
                                {comicData.characters.items.map((character) => {
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
                break;
            case "stories":
                if (comicData.stories)
                    if (comicData.stories.items)
                        setCardTabComponent(
                            <>
                                {comicData.stories.items.map((story) => {
                                    let storyID = story.resourceURI.substr(story.resourceURI.lastIndexOf('/'));
                                    return (
                                        <ListGroup.Item key={storyID}>
                                            <Card.Link href={`http://localhost:3000/stories${storyID}`}>{story.name}</Card.Link>
                                        </ListGroup.Item>
                                    )
                                })}
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
        let thumbnail = (comicData.thumbnail) ? `${comicData.thumbnail.path}.${comicData.thumbnail.extension}` : "https://i.annihil.us/u/prod/marvel/i/mg/b/40/image_not_available.jpg";
        return (
            <Card className="detailsCard">
                <Card.Header><h1>{comicData.title}</h1></Card.Header>
                <Card.Img alt={comicData.title} src={thumbnail} />
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

export default ComicDetails;