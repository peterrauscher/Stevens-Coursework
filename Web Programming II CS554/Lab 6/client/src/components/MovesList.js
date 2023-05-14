const MovesList = (props) => {
  function titleCase(str) {
    return str
      .toLowerCase()
      .split(" ")
      .map(function (word) {
        return word.replace(word[0], word[0].toUpperCase());
      })
      .join(" ");
  }

  const movesList = props.moves.map((move) => {
    return (
      <div className="typeItem" key={move.toString()}>
        <img
          src={encodeURI(
            `https://img.shields.io/badge/-${titleCase(
              move.toString().replaceAll("-", " ")
            )}-4C4E52`
          )}
        />
      </div>
    );
  });
  return <div className="typesContainer">{movesList}</div>;
};

export default MovesList;
