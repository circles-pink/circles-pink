import React, { ReactElement, useContext, useEffect, useState } from 'react';
import { CirclesState } from 'generated/output/CirclesPink.Garden.StateMachine.State';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type AnimState = {
  cur: CirclesState['type'];
  prev: CirclesState['type'];
  lastAction: number;
};

type AnimProviderProps = { state: CirclesState; children: ReactElement };

const initAnimState: AnimState = {
  cur: 'infoGeneral',
  prev: 'infoGeneral',
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
    cur: state.type,
    prev: state.type,
    lastAction: 0,
  });

  useEffect(() => {
    setAnimState(s => ({
      cur: state.type,
      prev: s.cur,
      lastAction: performance.now(),
    }));
  }, [state.type]);

  return animState;
};

// -----------------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------------

export const stateToIndex = (state: CirclesState): number | null => {
  switch (state.type) {
    case 'infoGeneral':
      return 0;
    case 'askUsername':
      return 1;
    case 'askEmail':
      return 2;
    case 'infoSecurity':
      return 3;
    case 'magicWords':
      return 4;
    case 'submit':
      return 5;
    default:
      return null;
  }
};
