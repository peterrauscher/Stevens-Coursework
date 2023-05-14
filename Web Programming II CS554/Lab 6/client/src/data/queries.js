import { gql } from "@apollo/client";

let exported = {
  GET_POKEMON_DETAILS: gql`
    query PokemonById($pokemonId: Int!) {
      pokemonById(id: $pokemonId) {
        id
        name
        thumbnail
        height
        weight
        moves
        sprites
        abilities
        types
        hp
        attack
        defense
        special_attack
        special_defense
        speed
      }
    }
  `,
  GET_POKEMON_PAGE: gql`
    query Query($pageNum: Int) {
      pokemonByPageNumber(pageNum: $pageNum) {
        thumbnail
        name
        id
      }
    }
  `,
  GET_NUMBER_OF_PAGES: gql`
    query Query {
      pokemonPageCount
    }
  `,
  GET_POKEMON_BY_SEARCH: gql`
    query PokemonBySearch($search: String) {
      pokemonBySearch(search: $search) {
        id
        name
        thumbnail
        height
        weight
        moves
        sprites
        abilities
        types
        hp
        attack
        defense
        special_attack
        special_defense
        speed
      }
    }
  `,
};

export default exported;
