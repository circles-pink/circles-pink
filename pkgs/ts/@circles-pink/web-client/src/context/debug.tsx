import React, { Dispatch, ReactElement, SetStateAction, useState } from 'react';

export const DebugContext: React.Context<[boolean, Dispatch<boolean>]> =
  React.createContext(
    // eslint-disable-next-line @typescript-eslint/no-empty-function
    [false, () => {}]
  );

type DebugProviderProps = {
  children: ReactElement;
};

export const DebugProvider = ({ children }: DebugProviderProps) => {
  const [debug, setDebug] = useState(false);

  return (
    <DebugContext.Provider value={[debug, setDebug]}>
      {children}
    </DebugContext.Provider>
  );
};
