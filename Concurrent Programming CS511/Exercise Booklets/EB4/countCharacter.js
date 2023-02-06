s =
  "AEBHFIGECHIDABFGECDHAIBFGECDHAIBFGECDHAIBFGECHDIABFGEHICDAFBGECHDIABFGECHDIABFGECDHAIBFGECDHAIBFGECDHAIBFGCEDAHBIFGECDAHBIFGEHCIDABFGCEDAHBIFGCEDAHBIFGECDHAIBFGCEDHAIBFGECDHAIBFGECDHAIBFGECDHAIBFGECDHAIBFGECDHAIBFGECDHAIBFGECHDIABFGECDHAIBFGECDHAIBFGECDHAIBFGCEDAHBIFGECHDIABFGECDHAIBFGCEDAHBIFGECDHAIBFGECDHAIBFGCEDAHBIFGECDAHBIFGECDHABIFGEHICDABFGEHICDABFGECHDIABFGECDHIABFGECHDIABFGECDHAIBFGECDHAIBFGECDHAIFBGECDHAIBFGECHDIABFGECDHAIBFGECDHAIBFGCD";

function printans(ans) {
  for (let [key, value] of ans) {
    // if()
    console.log(`${key}  occurs  ${value} times`);
  }
}

// function count occurrence of character
function count(str, outp_map) {
  for (let i = 0; i < str.length; i++) {
    let k = outp_map.get(str[i]);
    outp_map.set(str[i], k + 1);
  }
  //calling  print function
  printans(outp_map);
}

//function create map to count character
function count_occurs(test, callback) {
  //checking string is valid or not
  if (test.length === 0) {
    console.log(" empty string ");
    return;
  } else {
    // map for storing count values
    let ans = new Map();
    for (let i = 0; i < test.length; i++) {
      ans.set(test[i], 0);
    }

    callback(test, ans);
  }
}

count_occurs(s, count);
