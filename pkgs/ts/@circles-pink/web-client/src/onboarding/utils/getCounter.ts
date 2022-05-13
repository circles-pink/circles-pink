export const getCounter = (init: number) => {
  let counter = init;
  return (): number => {
    return ++counter;
  };
};

export const getIncrementor = (init: number, increment: number) => {
  const counter = getCounter(init);
  return () => counter() * increment;
};
