import React, { ReactElement, useState } from 'react';

export const DebugContext: React.Context<[boolean, (x: boolean) => void]> =
  React.createContext([false as boolean, (x: boolean) => {}]);

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
