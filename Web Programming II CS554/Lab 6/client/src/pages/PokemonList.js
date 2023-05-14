import { useEffect, useState } from "react";
import { useQuery, useLazyQuery } from "@apollo/client";
import { Alert, Button, Col, Card, Row } from "react-bootstrap";
import Error from "../components/Error";
import Loading from "../components/Loading";
import Search from "../components/Search";
import queries from "../data/queries";
import { useDispatch } from "react-redux";
import { catchRelease } from "../redux/slices/trainersSlice";
import { useSelector } from "react-redux";
import PokePagination from "../components/PokePagination";
import { Link, useParams } from "react-router-dom";

const PokemonList = () => {
  const [pokemonList, setPokemonList] = useState([]);
  const [pageNum, setPageNum] = useState(0);
  const [maxPageNum, setMaxPageNum] = useState(0);
  const [showAlert, setShowAlert] = useState(true);
  const [searchTerm, setSearchTerm] = useState("");
  const [activeTrainer, setActiveTrainer] = useState(undefined);
  const dispatch = useDispatch();
  const allTrainers = useSelector((state) => state.trainers.trainers);
  const { pagenum } = useParams();
  const [
    getNumOfPages,
    { loading: firstLoading, error: firstError, data: firstData },
  ] = useLazyQuery(queries.GET_NUMBER_OF_PAGES, {
    fetchPolicy: "network-only",
  });
  const {
    loading: secondLoading,
    error: secondError,
    data: secondData,
  } = useQuery(queries.GET_POKEMON_PAGE, {
    variables: { pageNum: parseInt(pagenum) },
    fetchPolicy: "network-only",
  });
  const [
    getPokemonBySearch,
    { loading: thirdLoading, error: thirdError, data: thirdData },
  ] = useLazyQuery(queries.GET_POKEMON_BY_SEARCH, {
    fetchPolicy: "network-only",
  });

  useEffect(() => {
    if (maxPageNum === 0) {
      getNumOfPages();
    }
  }, []);

  useEffect(() => {
    setPageNum(parseInt(pagenum));
  }, [pagenum]);

  useEffect(() => {
    if (firstData) setMaxPageNum(parseInt(firstData.pokemonPageCount) - 1);
  }, [firstData]);

  useEffect(() => {
    if (searchTerm.trim() !== "")
      getPokemonBySearch({ variables: { search: searchTerm } });
    if (thirdData && searchTerm !== "")
      setPokemonList(thirdData.pokemonBySearch);
    else if (secondData) setPokemonList(secondData.pokemonByPageNumber);
  }, [secondData, thirdData, searchTerm]);

  useEffect(() => {
    let alertRunningAnd = true;
    setActiveTrainer(
      allTrainers.find((trainer) => {
        if (trainer.isActiveTrainer) {
          alertRunningAnd = false;
          return true;
        }
      })
    );
    setShowAlert(alertRunningAnd);
  }, [allTrainers]);

  if (firstError || secondError || thirdError)
    return <Error message="Error 404: No Pokemon on this page!" />;

  const handleCatchRelease = (pokemonId) => dispatch(catchRelease(pokemonId));
  const searchValue = async (value) => setSearchTerm(value.toLowerCase());

  const pokemonCards = pokemonList.map((pokemon) => {
    return (
      <Col key={pokemon.id} className="card-column">
        <Card>
          <Link to={`/pokemon/${pokemon.id}`}>
            <Card.Img
              alt={pokemon.name}
              variant="top"
              src={pokemon.thumbnail}
            />
          </Link>
          <Card.Body>
            <Card.Title>
              {pokemon.name.charAt(0).toUpperCase() + pokemon.name.slice(1)}
            </Card.Title>
            <Button
              variant={!activeTrainer ? "secondary" : "primary"}
              onClick={() => handleCatchRelease(pokemon.id)}
            >
              {activeTrainer && activeTrainer.team.includes(pokemon.id)
                ? "Release!"
                : "Catch!"}
            </Button>
          </Card.Body>
        </Card>
      </Col>
    );
  });

  if (pokemonList)
    return (
      <>
        <Alert variant="warning" hidden={!showAlert}>
          No trainer is selected! You won't be able to build a team until you
          select one.
        </Alert>
        {searchTerm === "" && (
          <PokePagination pagenum={pageNum} maxpagenum={maxPageNum} />
        )}
        <Search placeholder="Search by name..." searchValue={searchValue} />
        {firstLoading || secondLoading ? (
          <Loading />
        ) : (
          <Row className="card-row" xs={2} sm={3} md={4} lg={5} xl={6}>
            {pokemonCards}
          </Row>
        )}
      </>
    );
};

export default PokemonList;
