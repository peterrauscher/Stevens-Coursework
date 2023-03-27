import { gql } from "@apollo/client";

let exported = {
  GET_IMAGES: gql`
    query UnsplashImages($pageNum: Int) {
      unsplashImages(pageNum: $pageNum) {
        id
        url
        posterName
        description
        userPosted
        binned
      }
    }
  `,
  GET_BINNED_POSTS: gql`
    query BinnedImages {
      binnedImages {
        id
        url
        posterName
        description
        userPosted
        binned
      }
    }
  `,
  GET_USER_POSTS: gql`
    query UserPostedImages {
      userPostedImages {
        id
        url
        posterName
        description
        userPosted
        binned
      }
    }
  `,
  GET_TOP_POSTS: gql`
    query TopTenBinnedPosts {
      getTopTenBinnedPosts {
        id
        url
        posterName
        description
        userPosted
        binned
        numBinned
      }
    }
  `,
  DELETE_IMAGE: gql`
    mutation DeleteImage($id: ID!) {
      deleteImage(id: $id) {
        id
        url
        posterName
        description
        userPosted
        binned
      }
    }
  `,
  UPDATE_IMAGE: gql`
    mutation UpdateImage(
      $id: ID!
      $url: String
      $posterName: String
      $description: String
      $userPosted: Boolean
      $binned: Boolean
    ) {
      updateImage(
        id: $id
        url: $url
        posterName: $posterName
        description: $description
        userPosted: $userPosted
        binned: $binned
      ) {
        id
        url
        posterName
        description
        userPosted
        binned
      }
    }
  `,
  UPLOAD_IMAGE: gql`
    mutation UploadImage(
      $url: String!
      $description: String
      $posterName: String
    ) {
      uploadImage(
        url: $url
        description: $description
        posterName: $posterName
      ) {
        id
        url
        posterName
        description
        userPosted
        binned
      }
    }
  `,
};

export default exported;
