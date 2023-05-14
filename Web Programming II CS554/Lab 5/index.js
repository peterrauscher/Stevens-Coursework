const { ApolloServer, gql } = require("apollo-server");
const uuid = require("uuid");
const axios = require("axios");
const redis = require("redis");
const client = redis.createClient();
const flat = require("flat");
const unflatten = flat.unflatten;

const auth = require("./authorization.js");
const validation = require("./client/src/data/validation");

const typeDefs = gql`
  type Query {
    unsplashImages(pageNum: Int): [ImagePost]
    binnedImages: [ImagePost]
    userPostedImages: [ImagePost]
  }
  type ImagePost {
    id: ID!
    url: String!
    posterName: String!
    description: String
    userPosted: Boolean!
    binned: Boolean!
  }
  type Mutation {
    uploadImage(
      url: String!
      description: String
      posterName: String
    ): ImagePost
    updateImage(
      id: ID!
      url: String
      posterName: String
      description: String
      userPosted: Boolean
      binned: Boolean
    ): ImagePost
    deleteImage(id: ID!): ImagePost
  }
`;

const resolvers = {
  Query: {
    unsplashImages: async (_, args) => {
      try {
        const { data } = await axios.get(auth.getPhotosByPageNum(args.pageNum));
        return data.map(async (result) => {
          let isBinned = await client.hExists("binnedimages", result.id);
          const post = {
            id: result.id,
            url: result.urls.regular,
            posterName: result.user.name,
            description: result.description,
            userPosted: false,
            binned: isBinned,
          };
          return post;
        });
      } catch (e) {
        console.error(e);
        throw `Failed to get unsplash images: ${e.message}`;
      }
    },
    binnedImages: async (_, args) => {
      try {
        const binned = await client.hGetAll("binnedimages");
        return unflatten(Object.values(binned).map((v) => JSON.parse(v)));
      } catch (e) {
        console.error(e);
        throw `Failed to get binned images: ${e.message}`;
      }
    },
    userPostedImages: async (_, args) => {
      try {
        const userPosts = await client.hGetAll("userposts");
        return unflatten(Object.values(userPosts).map((v) => JSON.parse(v)));
      } catch (e) {
        console.error(e);
        throw `Failed to get user posts: ${e.message}`;
      }
    },
  },
  Mutation: {
    uploadImage: async (_, args) => {
      try {
        await validation.checkImageURL(args.url);
        await validation.checkDescription(args.description);
        await validation.checkPosterName(args.posterName);
        const post = {
          id: uuid.v4(),
          url: args.url,
          posterName: args.posterName,
          description: args.description,
          userPosted: true,
          binned: false,
        };
        const uuidConflicts = await client.hExists("userposts", post.id);
        if (uuidConflicts)
          throw `Failed to upload image, generated uuid ${post.id} already exists`;
        else {
          await client.hSet("userposts", post.id, JSON.stringify(flat(post)));
          return post;
        }
      } catch (e) {
        console.error(e);
      }
    },
    updateImage: async (_, args) => {
      try {
        await validation.checkID(args.id);
        await validation.checkImageURL(args.url);
        await validation.checkBinned(args.binned);
        await validation.checkUserPosted(args.userPosted);
        const editedPost = {
          id: args.id,
          url: args.url,
          posterName: args.posterName,
          description: args.description,
          userPosted: args.userPosted,
          binned: args.binned,
        };
        if (editedPost.binned)
          await client.hSet(
            "binnedimages",
            editedPost.id,
            JSON.stringify(flat(editedPost))
          );
        else await client.hDel("binnedimages", editedPost.id);
        if (editedPost.userPosted)
          await client.hSet(
            "userposts",
            editedPost.id,
            JSON.stringify(flat(editedPost))
          );
        return editedPost;
      } catch (e) {
        console.error(e);
      }
    },
    deleteImage: async (_, args) => {
      try {
        await validation.checkID(args.id);
        let postExists = client.hExists("userposts", args.id);
        if (postExists) {
          await client.hDel("userposts", args.id);
          await client.hDel("binnedimages", args.id);
        } else
          throw `Failed to delete post with id ${args.id}, it was not user-posted or not found`;
      } catch (e) {
        console.error(e);
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
