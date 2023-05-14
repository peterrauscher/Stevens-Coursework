const md5 = require("blueimp-md5");
const publickey = "ee1720945ad0f3e3507f206b23c880d9";
const privatekey = "784bf83c34b253b35e4cd572726b83aa7073a18f";

let getAuthorizedUrl = (baseUrl) => {
    const ts = new Date().getTime();
    const hash = md5(ts + privatekey + publickey);
    return `${baseUrl}?ts=${ts}&apikey=${publickey}&hash=${hash}`;
};

let getAuthorizedSearchUrl = (baseUrl, search) => {
    const ts = new Date().getTime();
    const hash = md5(ts + privatekey + publickey);
    return `${baseUrl}?nameStartsWith=${encodeURIComponent(search)}&limit=100&ts=${ts}&apikey=${publickey}&hash=${hash}`;
};

let getAuthorizedSearchByCharacterUrl = (baseUrl, search) => {
    const ts = new Date().getTime();
    const hash = md5(ts + privatekey + publickey);
    return `${baseUrl}?characters=${encodeURIComponent(search)}&limit=100&ts=${ts}&apikey=${publickey}&hash=${hash}`;
};

let getAuthorizedSearchByComicUrl = (baseUrl, search) => {
    const ts = new Date().getTime();
    const hash = md5(ts + privatekey + publickey);
    return `${baseUrl}?comics=${encodeURIComponent(search)}&limit=100&ts=${ts}&apikey=${publickey}&hash=${hash}`;
};

let getAuthorizedSearchForComicsUrl = (baseUrl, search) => {
    const ts = new Date().getTime();
    const hash = md5(ts + privatekey + publickey);
    return `${baseUrl}?titleStartsWith=${encodeURIComponent(search)}&limit=100&ts=${ts}&apikey=${publickey}&hash=${hash}`;
};

module.exports = {
    getAuthorizedUrl,
    getAuthorizedSearchUrl,
    getAuthorizedSearchByCharacterUrl,
    getAuthorizedSearchByComicUrl,
    getAuthorizedSearchForComicsUrl,
}
