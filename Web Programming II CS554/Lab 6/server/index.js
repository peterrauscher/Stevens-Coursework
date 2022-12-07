const { ApolloServer, gql } = require("apollo-server");
const uuid = require("uuid");
const axios = require("axios");
const redis = require("redis");
const fs = require("fs");
const client = redis.createClient();
const flat = require("flat");
const unflatten = flat.unflatten;
const validate = require("./validation");
const idRegex = new RegExp("(https://pokeapi.co/api/v2/pokemon/)|(/)", "g");
const RESULTS_PER_PAGE = 50;

const typeDefs = gql`
  type Query {
    pokemonPageCount: Int
    pokemonByPageNumber(pageNum: Int): [Pokemon]
    pokemonById(id: Int): Pokemon
    pokemonBySearch(search: String): [Pokemon]
  }
  type Pokemon {
    id: Int!
    name: String!
    thumbnail: String!
    height: Int
    weight: Int
    moves: [String]
    sprites: [String]
    abilities: [String]
    types: [String]
    hp: Int
    attack: Int
    defense: Int
    special_attack: Int
    special_defense: Int
    speed: Int
  }
`;

const resolvers = {
  Query: {
    pokemonPageCount: async (_, args) => {
      try {
        let cached = await client.get("numofpages");
        if (cached !== null) {
          console.log(`Returned number of pages from cache`);
          return parseInt(cached);
        } else {
          const { data } = await axios.get(
            `https://pokeapi.co/api/v2/pokemon?offset=0&limit=1`
          );
          const numOfPages = Math.ceil(parseInt(data.count) / RESULTS_PER_PAGE);
          await client.set("numofpages", numOfPages);
          console.log(`Requested and cached number of pages`);
          return numOfPages;
        }
      } catch (e) {
        console.error(e);
        throw `Failed to get number of pages: ${e.message}`;
      }
    },
    pokemonByPageNumber: async (_, args) => {
      try {
        await validate.checkPageNumber(args.pageNum);
        let cached = await client.hGet("pokemonpages", args.pageNum.toString());
        if (cached !== null) {
          console.log(`Returned page ${args.pageNum} of pokemon from cache`);
          return Object.values(unflatten(JSON.parse(cached)));
        } else {
          const { data } = await axios.get(
            `https://pokeapi.co/api/v2/pokemon?offset=${
              RESULTS_PER_PAGE * parseInt(args.pageNum)
            }&limit=${RESULTS_PER_PAGE}`
          );
          const pokemonList = data.results.map((result) => {
            const id = parseInt(result.url.replaceAll(idRegex, ""));
            const thumbnailPath = `/official-artwork/${id}.png`;
            const pokemon = {
              id: id,
              name: result.name,
              thumbnail: fs.existsSync(`../client/public${thumbnailPath}`)
                ? thumbnailPath
                : "/no-image.png",
            };
            return pokemon;
          });
          if (pokemonList.length === 0) throw `No pokemon found on this page!`;
          await client.hSet(
            "pokemonpages",
            args.pageNum.toString(),
            JSON.stringify(flat(pokemonList))
          );
          console.log(`Request and cached page ${args.pageNum} of pokemon`);
          return pokemonList;
        }
      } catch (e) {
        console.error(e);
        throw `Failed to get page ${args.pageNum}: ${e.message}`;
      }
    },
    pokemonById: async (_, args) => {
      try {
        await validate.checkId(args.id);
        let cached = await client.hGet("pokemondetails", args.id.toString());
        if (cached !== null) {
          console.log(`Returned pokemon with id ${args.id} from cache`);
          return unflatten(JSON.parse(cached));
        } else {
          const { data } = await axios.get(
            `https://pokeapi.co/api/v2/pokemon/${args.id}`
          );
          const id = parseInt(data.id);
          if (parseInt(args.id) !== id)
            throw `ID of API response does not match query parameter`;
          console.log(`Requested and cached pokemon with id ${args.id}`);
          const thumbnailPath = `/official-artwork/${id}.png`;
          const pokemonDetails = {
            id: id,
            name: data.name,
            thumbnail: fs.existsSync(`../client/public${thumbnailPath}`)
              ? thumbnailPath
              : "/no-image.png",
            height: data.height,
            weight: data.weight,
            moves: data.moves.map((move) => move.move.name),
            sprites: [
              data.sprites.front_default,
              data.sprites.back_default,
              data.sprites.front_shiny,
              data.sprites.back_shiny,
            ],
            abilities: data.abilities.map((ability) => ability.ability.name),
            types: data.types.map((type) => type.type.name),
            hp: data.stats.filter((stat) => stat.stat.name == "hp")[0]
              .base_stat,
            attack: data.stats.filter((stat) => stat.stat.name == "attack")[0]
              .base_stat,
            defense: data.stats.filter((stat) => stat.stat.name == "defense")[0]
              .base_stat,
            special_attack: data.stats.filter(
              (stat) => stat.stat.name == "special-attack"
            )[0].base_stat,
            special_defense: data.stats.filter(
              (stat) => stat.stat.name == "special-defense"
            )[0].base_stat,
            speed: data.stats.filter((stat) => stat.stat.name == "speed")[0]
              .base_stat,
          };
          await client.hSet(
            "pokemondetails",
            args.id.toString(),
            JSON.stringify(flat(pokemonDetails))
          );
          return pokemonDetails;
        }
      } catch (e) {
        console.error(e);
        throw `Failed to get pokemon with id ${args.pageNum}: ${e.message}`;
      }
    },
    pokemonBySearch: async (_, args) => {
      try {
        await validate.checkSearchTerm(args.search);
        let cached = await client.get("allpokemon");
        let allPokemon = [];
        if (cached !== null) {
          console.log(`Returned searchable pokemon from cache`);
          allPokemon = Object.values(unflatten(JSON.parse(cached)));
        } else {
          const { data } = await axios.get(
            `https://pokeapi.co/api/v2/pokemon?limit=100000&offset=0`
          );
          console.log(`Requested and cached all searchable pokemon`);
          await client.set("allpokemon", JSON.stringify(flat(data.results)));
          allPokemon = data.results;
        }
        const searchResults = allPokemon.reduce((runningArray, pokemon) => {
          let searchRegex = new RegExp(args.search.toString().trim(), "g");
          if (searchRegex.test(pokemon.name.toString())) {
            const id = parseInt(pokemon.url.replaceAll(idRegex, ""));
            const thumbnailPath = `/official-artwork/${id}.png`;
            const pokemonGql = {
              id: id,
              name: pokemon.name,
              thumbnail: fs.existsSync(`../client/public${thumbnailPath}`)
                ? thumbnailPath
                : "/no-image.png",
            };
            return [...runningArray, pokemonGql];
          } else return runningArray;
        }, []);
        return searchResults;
      } catch (e) {
        console.error(e);
        throw `Failed to search for pokemon with term ${args.search}: ${e.message}`;
      }
    },
  },
};

const server = new ApolloServer({
  typeDefs,
  resolvers,
});

(async () => {
  client.on("error", (err) => console.log("Redis Client Error", err));
  await client.connect();
  server.listen().then(({ url }) => {
    console.log(`🚀 Server ready at: ${url}`);
  });
})();
