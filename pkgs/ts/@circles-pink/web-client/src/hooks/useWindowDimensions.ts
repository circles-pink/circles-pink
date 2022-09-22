import { useState, useEffect } from 'react';

type WindowDimensions = {
  width: number;
  height: number;
};

export const useWindowDimensions = (): WindowDimensions => {
  const hasWindow = typeof window !== 'undefined';

  const getWindowDimensions = (): WindowDimensions => {
    const width = hasWindow ? window.innerWidth : 0;
    const height = hasWindow ? window.innerHeight : 0;
    return {
      width,
      height,
    };
  };

  const handleResize = (): void => {
    setWindowDimensions(getWindowDimensions());
  };

  const [windowDimensions, setWindowDimensions] = useState<WindowDimensions>(
    getWindowDimensions()
  );

  useEffect(() => {
    if (hasWindow) {
      window.addEventListener('resize', handleResize);
      return () => window.removeEventListener('resize', handleResize);
    }
  }, [hasWindow]);

  return windowDimensions;
};
