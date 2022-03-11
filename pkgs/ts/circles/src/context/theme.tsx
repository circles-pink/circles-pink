import React, { ReactElement, useState } from 'react';
import chroma from 'chroma-js';

type Theme = {
  baseColor: string;
};

const defaultTheme: Theme = {
  baseColor: chroma('hotpink').hex(),
};

type UpdateType = React.Dispatch<React.SetStateAction<typeof defaultTheme>>;

const setTheme: UpdateType = () => defaultTheme;

export const ThemeContext = React.createContext({
  theme: defaultTheme,
  setTheme,
});

type ThemeProviderProps = {
  children: ReactElement;
};

export const ThemeProvider = ({ children }: ThemeProviderProps) => {
  const [theme, setTheme] = useState(defaultTheme);

  return (
    <ThemeContext.Provider value={{ theme, setTheme }}>
      {children}
    </ThemeContext.Provider>
  );
};
