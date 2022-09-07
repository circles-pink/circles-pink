import { CirclesState } from '@circles-pink/state-machine/src';
import React, { ReactElement, useContext, useEffect, useState } from 'react';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

export type AnimState = {
  selected: CirclesState['type'];
  prevSelected: CirclesState['type'];
  lastAction: number;
};

type AnimProviderProps = { state: CirclesState; children: ReactElement };

const initAnimState: AnimState = {
  selected: 'askUsername',
  prevSelected: 'askUsername',
  lastAction: 0,
};

// -----------------------------------------------------------------------------
// Context
// -----------------------------------------------------------------------------

const AnimContext: React.Context<AnimState> =
  React.createContext(initAnimState);

export const useAnimContext = () => useContext(AnimContext);

export const AnimProvider = ({ state, children }: AnimProviderProps) => {
  const animState = useAnimState(state);

  return (
    <AnimContext.Provider value={animState}>{children}</AnimContext.Provider>
  );
};

// -----------------------------------------------------------------------------
// Hooks
// -----------------------------------------------------------------------------

const useAnimState = (state: CirclesState): AnimState => {
  const [animState, setAnimState] = useState<AnimState>({
    selected: state.type,
    prevSelected: state.type,
    lastAction: 0,
  });

  useEffect(() => {
    setAnimState(s => ({
      selected: state.type,
      prevSelected: s.selected,
      lastAction: performance.timeOrigin + performance.now(),
    }));
  }, [state.type]);

  return animState;
};
