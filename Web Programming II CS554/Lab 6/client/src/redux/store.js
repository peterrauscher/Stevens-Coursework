import { configureStore } from "@reduxjs/toolkit";
import trainersSlice from "./slices/trainersSlice";
export default configureStore({
  reducer: {
    trainers: trainersSlice,
  },
});
