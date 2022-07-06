import React, { useEffect, useState } from 'react';
import styled from '@emotion/styled';
import tw from 'twin.macro';
import { Checkbox } from './forms';

type StateMachineDebuggerProps<K extends string, V> = {
  state: Record<K, V>;
};

export const StateMachineDebugger = ({
  state,
}: StateMachineDebuggerProps<string, any>) => {
  const stateKeys = Object.keys(state).sort((a, b) => a.localeCompare(b));

  const [filters, setFilters] = useState<Record<string, boolean>>(
    Object.fromEntries(stateKeys.map(k => [k, false])) as Record<
      string,
      boolean
    >
  );

  const updateFilter = (key: string) =>
    setFilters({ ...filters, [key]: !filters[key] });

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
