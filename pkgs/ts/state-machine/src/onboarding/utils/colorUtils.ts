import chroma from 'chroma-js';

export const darken = (color: string, by?: number): string =>
  chroma(color)
    .darken(by || 0.25)
    .hex();

export const lighten = (color: string, by?: number): string =>
  chroma(color)
    .luminance(by || 0.9)
    .hex();
