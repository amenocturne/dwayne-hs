/**
 * Store
 * 
 * Manages application state and coordinates the update cycle.
 * Provides the dispatch function that drives all state changes.
 */

import type { AppState } from "../types/state.js";
import type { Action } from "./actions.js";
import { update } from "./reducer.js";
import { runEffect, type Dispatch } from "./effects.js";

export interface Store {
  readonly dispatch: Dispatch;
  readonly getState: () => AppState;
}

/**
 * Creates the application store.
 * 
 * The store:
 * 1. Holds current state
 * 2. Provides dispatch function to trigger state transitions
 * 3. Calls render after each state change
 * 4. Executes effects returned from the reducer
 */
export function createStore(
  initialState: AppState,
  render: (state: AppState) => void
): Store {
  let state = initialState;

  const dispatch: Dispatch = (action: Action) => {
    const [newState, effect] = update(state, action);
    state = newState;
    render(state);

    if (effect.type !== 'None') {
      runEffect(effect, dispatch);
    }
  };

  return {
    dispatch,
    getState: () => state,
  };
}
