const md5 = require("blueimp-md5");
const publickey = "ee1720945ad0f3e3507f206b23c880d9";
const privatekey = "784bf83c34b253b35e4cd572726b83aa7073a18f";

let getAuthorizedUrl = (baseUrl) => {
  const ts = new Date().getTime();
  const hash = md5(ts + privatekey + publickey);
  return `${baseUrl}?ts=${ts}&apikey=${publickey}&hash=${hash}`;
};

let getAuthorizedUrlWithPagination = (baseUrl, offset, limit) => {
  const ts = new Date().getTime();
  const hash = md5(ts + privatekey + publickey);
  return `${baseUrl}?limit=${limit}&offset=${offset}&ts=${ts}&apikey=${publickey}&hash=${hash}`;
};

module.exports = {
  getAuthorizedUrl,
  getAuthorizedUrlWithPagination,
};
