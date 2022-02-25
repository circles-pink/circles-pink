// Inspired by: https://medium.com/@colebemis/building-a-checkbox-component-with-react-and-styled-components-8d3aa1d826dd

import React, { ReactElement } from 'react';
import { styled } from 'twin.macro';

type LabeledCheckboxProps = {
  checked: boolean;
  setChecked: () => void;
  label?: string;
};

export const LabeledCheckbox = ({
  checked = false,
  setChecked,
  label,
}: LabeledCheckboxProps): ReactElement => {
  return (
    <div>
      <Label>
        <Checkbox checked={checked} onChange={() => setChecked()} />
        <span>{label}</span>
      </Label>
    </div>
  );
};

type CheckboxProps = {
  checked: boolean;
  onChange: () => void;
};

const Checkbox = ({ checked, ...props }: CheckboxProps) => (
  <CheckboxContainer>
    <HiddenCheckbox checked={checked} {...props} />
    <StyledCheckbox checked={checked}>
      {checked && (
        <Icon viewBox="0 0 24 24">
          <polyline points="20 6 9 17 4 12" />
        </Icon>
      )}
    </StyledCheckbox>
  </CheckboxContainer>
);

const Label = styled.label`
  cursor: pointer;
`;

const HiddenCheckbox = styled.input`
  // Hide checkbox visually but remain accessible to screen readers.
  // Source: https://polished.js.org/docs/#hidevisually
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  white-space: nowrap; /* added line */
  border: 0;
`;
HiddenCheckbox.defaultProps = { type: 'checkbox' };

const Icon = styled.svg`
  fill: none;
  stroke: white;
  stroke-width: 2px;
  margin: 0;
  padding: 0;
`;

type StyledCheckboxProps = {
  checked: boolean;
};

const StyledCheckbox = styled.div<StyledCheckboxProps>`
  display: inline-block;
  width: 16px;
  height: 16px;
  background: ${props => (props.checked ? '#ff4785' : 'white')};
  border-radius: 3px;
  transition: all 150ms;
  border: 1px solid pink;
`;

const CheckboxContainer = styled.div`
  display: inline-block;
  vertical-align: middle;
  margin-right: 0.5rem;
  height: 17px;
`;
