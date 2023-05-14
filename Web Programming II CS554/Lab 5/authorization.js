const API_ACCESS_KEY = 'k3LZH1hOT-SfJoW7nGRo21JiMUJ4zUJYpgKO6B1Jp8E';

const getPhotosByPageNum = (pageNum) => {
    return `https://api.unsplash.com/photos?page=${pageNum}&client_id=${API_ACCESS_KEY}`;
}

module.exports = {
    getPhotosByPageNum,
}