import queries from "../data/queries";
import PostsList from "./PostsList";

const Images = () => {
  return <PostsList query={queries.GET_IMAGES} type="Images" />;
};

export default Images;
