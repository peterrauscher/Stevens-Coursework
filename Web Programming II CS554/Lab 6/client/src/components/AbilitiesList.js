const AbilitiesList = (props) => {
  function titleCase(str) {
    return str
      .toLowerCase()
      .split(" ")
      .map(function (word) {
        return word.replace(word[0], word[0].toUpperCase());
      })
      .join(" ");
  }

  const abilitiesList = props.abilities.map((ability) => {
    return (
      <div className="typeItem" key={ability.toString()}>
        <img
          src={encodeURI(
            `https://img.shields.io/badge/-${titleCase(
              ability.toString().replaceAll("-", " ")
            )}-4C4E52`
          )}
        />
      </div>
    );
  });
  return <div className="typesContainer">{abilitiesList}</div>;
};

export default AbilitiesList;
