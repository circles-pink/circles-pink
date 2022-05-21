import React, { useEffect, useState } from 'react';
import { DefaultView } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import styled from '@emotion/styled';
import tw from 'twin.macro';
import { Checkbox } from './forms';

type StateMachineDebuggerProps = {
  state: DefaultView;
};

export const StateMachineDebugger = ({ state }: StateMachineDebuggerProps) => {
  const [filters, setFilters] = useState<{ [key: string]: boolean }>({});

  const stateKeys = Object.keys(state).sort((a, b) => a.localeCompare(b));

  useEffect(() => {
    const newFilters = {};
    stateKeys.forEach(k => (newFilters[k] = k in filters || true));
    setFilters(newFilters);
  }, []);

  const updateFilter = (key: string) => {
    const update = {
      ...filters,
    };
    update[key] = !filters[key];
    setFilters(update);
  };

  return (
    <Frame>
      <FilterSetup>
        {stateKeys.map((k: string) => {
          return (
            <div key={k}>
              <Checkbox
                checked={filters[k]}
                setChecked={() => updateFilter(k)}
                background="pink"
                borderColor="hotpink"
                label={k}
              />
            </div>
          );
        })}
      </FilterSetup>
      {stateKeys
        .filter(k => filters[k])
        .map((k: string) => {
          return (
            <div key={k}>
              <b>{k}:</b>
              <pre>{JSON.stringify(state[k], null, 2)}</pre>
            </div>
          );
        })}
    </Frame>
  );
};

const Frame = styled.div(() => [tw`relative pb-8 min-h-screen`]);
const FilterSetup = styled.div(() => [tw`absolute right-0 top-0`]);
