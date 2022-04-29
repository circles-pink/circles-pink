import React, { ReactElement, useContext, useEffect, useState } from 'react';
import { CirclesState } from 'generated/output/CirclesPink.Garden.StateMachine.State';

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
  selected: 'infoGeneral',
  prevSelected: 'infoGeneral',
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

// -----------------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------------

export const stateToIndex = (
  stateType: CirclesState['type']
): number | undefined => {
  switch (stateType) {
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
      return undefined;
  }
};
