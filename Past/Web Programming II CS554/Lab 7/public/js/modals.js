const titles = {
  allamerican: "The All-American",
  newyorker: "The New Yorker",
  boston: "The Boston",
  detroitconey: "The Detroit-Coney",
  hawaiian: "The Hawaiian",
  jamaican: "The Jamaican",
  polish: "The Polish",
  blt: "The BLT",
  surfnturf: "The Surf N' Turf",
  bratwurst: "The Bratwurst",
};

const descriptions = {
  allamerican:
    "Nothing like a classic. Mustard, ketchup, relish, and onions bring a colorful rainbow of artery clogging to the plate.",
  newyorker:
    "Sauerkraut and mustard. Awfully German looking, but it isn't. Those dirty water doggers just do things right.",
  boston:
    "Everyone loves some baked beans at a barbecue. All jokes aside, putting them on the bun wasn't a bad call.",
  detroitconey:
    "The lovechild of a sloppy joe and The Boston, brings extra meat and extra cholesterol to the table.",
  hawaiian:
    "Hot dogs are just close enough to Spam that this is actually a pretty authentic dish!",
  jamaican:
    "Certain cylindrical objects from Jamaica usually come wrapped in something other than a bun, but this one's pretty good too.",
  polish:
    "A pretty odd mesh of flavors, but then again Poland is a pretty weird mesh of Germany and Russia. Throw on some Vodka and sauerkraut and it'll be World War Hotdog.",
  blt: "There's probably a human rights issue with wrapping a cylinder of ground pig in another pig's meat, but it tastes too good to care if it's a violation of the Geneva convention or not.",
  surfnturf:
    "Why enjoy a good lobster roll when you can taint that flavor with low-quality meat? Now you don't have to.",
  bratwurst:
    "The best looking hotdog on here, but that would be my cultural bias showing. Nothing beats sauerkraut.",
};

const cards = document.getElementsByClassName("more-btn");

[...cards].forEach((card) => {
  card.addEventListener("click", function (event) {
    let id = event.currentTarget.parentNode.parentNode.id;
    document.getElementById("detailsModalLabel").innerText = titles[id];
    document.getElementById("detailsModalBody").innerText = descriptions[id];
  });
});
