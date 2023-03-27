import queries from "../data/queries";
import PostsList from "./PostsList";

const Binned = () => {
  return <PostsList query={queries.GET_BINNED_POSTS} type="Binned" />;
};

export default Binned;
