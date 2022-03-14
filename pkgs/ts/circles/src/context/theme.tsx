import React, { ReactElement, useState } from 'react';
import chroma from 'chroma-js';

export type Theme = {
  baseColor: string;
};

const defaultTheme: Theme = {
  baseColor: chroma('hotpink').hex(),
};

export type ThemeContextType = {
  baseColor: string;
  setTheme: (theme: Theme) => void;
};

type SetColor = (color: string) => void;

export const ThemeContext: React.Context<[Theme, SetColor]> =
  React.createContext(
    // eslint-disable-next-line @typescript-eslint/no-unused-vars, @typescript-eslint/no-empty-function
    [defaultTheme, _ => {}]
  );

type ThemeProviderProps = {
  children: ReactElement;
};

export const ThemeProvider = ({ children }: ThemeProviderProps) => {
  const [theme, setThemeState] = useState<Theme>(defaultTheme);

  const setColor = (color: string) => {
    console.log(color);
    setThemeState({
      baseColor: chroma(color).hex(),
    });
    console.log(theme);
  };

  return (
    <ThemeContext.Provider value={[theme, setColor]}>
      {children}
    </ThemeContext.Provider>
  );
};
