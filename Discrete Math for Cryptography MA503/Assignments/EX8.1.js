let U5 = new Set([1, 2, 3, 4]);
let Z3 = new Set([0, 1, 2]);
let directProduct = new Set();
let result = new Set();

U5.forEach((Ui) => {
	Z3.forEach((Zi) => {
  	directProduct.add(`(${Ui}, ${Zi})`);
  });
});

let a = 3;
let b = 2;

for(let i = 0; i < 100; i++) {
	let pair = `(${(a*i)%5}, ${(b*i)%3})`;
	if (directProduct.has(pair)) {
  	if (result.has(pair)) {
    	console.log("Broke at ", i);
       break;
    }
    else result.add(pair);
  }
}

console.log([...result.values()]);