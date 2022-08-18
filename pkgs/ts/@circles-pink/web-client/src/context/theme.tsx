import React, { ReactElement, SetStateAction, useState } from 'react';
import chroma from 'chroma-js';

export type Theme = {
  baseColor: string;
  cardColor: string;
  darkColor: string;
  lightColor: string;
  textColorLight: string;
  textColorDark: string;
  bgColor: string;
};

export const defaultTheme: Theme = {
  baseColor: chroma('#FF69B4').hex(),
  cardColor: chroma('#CFD6EA').hex(),
  darkColor: chroma('#65655E').hex(),
  lightColor: chroma('#CFD6EA').hex(),
  textColorDark: chroma('#7e7e7e').hex(),
  textColorLight: chroma('#fff').hex(),
  bgColor: chroma('#f9fafb').hex(),
};

export const defaultThemeXbge: Theme = {
  baseColor: chroma('#FB8298').hex(),
  cardColor: chroma('#f9ccd4').hex(),
  darkColor: chroma('#65655E').hex(),
  lightColor: chroma('#f9ccd4').hex(),
  textColorDark: chroma('#7e7e7e').hex(),
  textColorLight: chroma('#fff').hex(),
  bgColor: chroma('#fff').hex(),
};

export type ThemeContextType = Theme & {
  setTheme: SetColor;
};

type SetColor = React.Dispatch<SetStateAction<Theme>>;

export const ThemeContext: React.Context<[Theme, SetColor]> =
  React.createContext(
    // eslint-disable-next-line @typescript-eslint/no-unused-vars, @typescript-eslint/no-empty-function
    [defaultTheme, _ => {}]
  );

type ThemeProviderProps = {
  children: ReactElement;
};

export const ThemeProvider = ({ children }: ThemeProviderProps) => {
  const [theme, setTheme] = useState<Theme>(defaultThemeXbge);

  return (
    <ThemeContext.Provider value={[theme, setTheme]}>
      {children}
    </ThemeContext.Provider>
  );
};
