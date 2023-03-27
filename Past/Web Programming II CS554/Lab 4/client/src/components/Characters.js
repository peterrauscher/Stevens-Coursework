import React, { useState, useEffect } from "react";
import axios from "axios";
import { Link, useParams } from "react-router-dom";
import Card from "react-bootstrap/Card";
import Row from "react-bootstrap/Row";
import Col from 'react-bootstrap/Col';
import Button from 'react-bootstrap/Button';
import SearchCatalog from "./SearchCatalog";
import Error from "./Error";
import auth from "../auth";
import PageSizeDropdown from "./PageSizeDropdown";
import usePersistedState from '../usePersistedState';

const Characters = () => {
    const regex = /(<([^>]+)>)/gi;
    const [loading, setLoading] = useState(true);
    const [charactersData, setCharactersData] = useState(undefined);
    const [searchTerm, setSearchTerm] = useState("");
    const [nextPageNumber, setNextPageNumber] = useState(0);
    const [previousPageNumber, setPreviousPageNumber] = useState(0);
    const [pageSize, setPageSize] = usePersistedState('pageSize', 20);
    const [error, setError] = useState(false);

    let { pagenum } = useParams();
    let cardColumns = null;
    let prevButton = null;
    let nextButton = null;
    const searchValue = async (value) => setSearchTerm(value);
    const pageSizeHandler = async (value) => setPageSize(value);

    // On component load
    useEffect(() => {
        async function fetchData() {
            try {
                let pageNumber = parseInt(pagenum);
                const { data } = await axios.get(`http://localhost:4000/api/characters/page/${pageNumber}?page_size=${pageSize}`);
                if (!data) throw "Could not get characters";
                setCharactersData(data.results);
                setLoading(false);
                if (pageNumber < 1 || pageNumber > data.maxPage) {
                    setNextPageNumber(0);
                    setPreviousPageNumber(0);
                    setError(true);
                } else if (pageNumber === data.maxPage) {
                    setNextPageNumber(0);
                    setPreviousPageNumber(pageNumber - 1);
                    setError(false);
                } else {
                    setNextPageNumber(pageNumber + 1);
                    setPreviousPageNumber(pageNumber - 1);
                    setError(false);
                }
            } catch (e) {
                setCharactersData(undefined);
                setPreviousPageNumber(0);
                setNextPageNumber(0);
                setError(true);
                setLoading(false);
                console.log(e);
            }
        }
        async function fetchSearchData() {
            try {
                const { data } = await axios.get(auth.getAuthorizedSearchUrl("https://gateway.marvel.com/v1/public/characters", searchTerm));
                if (!data) throw "Could not get characters by search term";
                setCharactersData(data.data.results);
                setLoading(false);
            } catch (e) {
                setCharactersData(undefined);
                setLoading(false);
                console.log(e);
            }
        }

        if (searchTerm) fetchSearchData();
        else fetchData();
    }, [pagenum, searchTerm, pageSize]);

    // Return error component when error is encountered
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

    if (!searchTerm) {
        prevButton = (() => {
            if (previousPageNumber)
                return (
                    <Link className="navButtons" to={`/characters/page/${previousPageNumber}`}>
                        <Button variant="danger">Previous</Button>
                    </Link>
                );
        })();
        nextButton = (() => {
            if (nextPageNumber)
                return (
                    <Link className="navButtons" to={`/characters/page/${nextPageNumber}`}>
                        <Button variant="danger">Next</Button>
                    </Link>
                );
        })();

    }

    if (loading) return <h1 className="pageTitle">Loading....</h1>
    else {
        return (
            <div>
                <div className="contentHeader">
                    <h1 className="pageTitle">Characters</h1>
                    <div className="searchAndNavBar">
                        <SearchCatalog placeholder="Search by name..." searchValue={searchValue} />
                        {prevButton}
                        {'  '}
                        {nextButton}
                        {'  '}
                        <PageSizeDropdown pageSizeHandler={pageSizeHandler} pageSizeValue={pageSize} />
                    </div>
                </div>
                <div className="contentBody">
                    <Row className="card-row" xs={1} md={3}>
                        {cardColumns}
                    </Row>
                </div >
            </div>
        );
    }
}

export default Characters;