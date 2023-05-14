import queries from "../data/queries";
import PostsList from "./PostsList";

const Posts = () => {
  return <PostsList query={queries.GET_USER_POSTS} type="Posts" />;
};

export default Posts;
