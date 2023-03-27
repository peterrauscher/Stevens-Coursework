const typeColors = {
  normal: "AAA99A",
  fighting: "BB5543",
  flying: "869AFF",
  poison: "AA5498",
  ground: "DEBB56",
  rock: "BBA966",
  bug: "A8BC22",
  ghost: "6665BC",
  steel: "AAAABA",
  fire: "FF4224",
  water: "3399FE",
  grass: "75CC55",
  electric: "FFCB33",
  psychic: "FE5699",
  ice: "66CCFF",
  dragon: "7766EE",
  dark: "775543",
  fairy: "EE98EE",
  unknown: "AAAA98",
  shadow: "4B0083",
};

const TypesList = (props) => {
  const typesList = props.types.map((type) => {
    return (
      <div className="typeItem" key={type.toString()}>
        <img
          src={`https://img.shields.io/badge/-${type
            .toString()
            .toUpperCase()}-${typeColors[type.toString()]}`}
        />
      </div>
    );
  });
  return <div className="typesContainer">{typesList}</div>;
};

export default TypesList;
