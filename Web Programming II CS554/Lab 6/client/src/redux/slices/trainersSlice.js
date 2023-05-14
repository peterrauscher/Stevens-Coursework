import { createSlice } from "@reduxjs/toolkit";
import { v4 as uuid } from "uuid";

export const trainersSlice = createSlice({
  name: "trainers",
  initialState: {
    trainers: [],
  },
  reducers: {
    createTrainer: (state, action) => {
      const { payload } = action;
      console.log("Created trainer: ", payload);
      state.trainers.push({
        id: uuid(),
        name: payload,
        isActiveTrainer: false,
        team: [],
      });
    },
    deleteTrainer: (state, action) => {
      const { payload } = action;
      console.log("Deleted trainer: ", payload);
      state.trainers = state.trainers.filter((t) => t.id !== payload);
    },
    activateTrainer: (state, action) => {
      const { payload } = action;
      console.log("Activated trainer: ", payload);
      state.trainers.forEach((trainer, i) => {
        if (trainer.id === payload) state.trainers[i].isActiveTrainer = true;
        else state.trainers[i].isActiveTrainer = false;
      });
    },
    deactivateTrainer: (state, action) => {
      const { payload } = action;
      console.log("Deactivated trainer: ", payload);
      state.trainers[
        state.trainers.findIndex((trainer) => trainer.id === payload)
      ].isActiveTrainer = false;
    },
    catchRelease: (state, action) => {
      const { payload } = action;

      let trainerIndex = state.trainers.findIndex(
        (trainer) => trainer.isActiveTrainer === true
      );
      let pokemonIndex = state.trainers[trainerIndex].team.indexOf(payload);
      if (pokemonIndex < 0)
        if (state.trainers[trainerIndex].team.length < 6) {
          if (!state.trainers[trainerIndex].team.includes(payload)) {
            console.log("Adding to team: ", payload);
            state.trainers[trainerIndex].team.push(payload);
          } else
            console.log(
              `Error: Trainer '${state.trainers[trainerIndex].name}' already has that pokemon!`
            );
        } else {
          console.log(
            `Error: Trainer '${state.trainers[trainerIndex].name}' already has a full team!`
          );
        }
      else {
        console.log("Removing from team: ", payload);
        state.trainers[trainerIndex].team.splice(pokemonIndex, 1);
      }
    },
  },
});

export const {
  createTrainer,
  deleteTrainer,
  activateTrainer,
  deactivateTrainer,
  catchRelease,
} = trainersSlice.actions;

export default trainersSlice.reducer;
