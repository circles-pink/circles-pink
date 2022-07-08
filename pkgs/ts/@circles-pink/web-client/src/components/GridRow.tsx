import React, { ReactElement } from 'react';
import { css, styled } from 'twin.macro';

type Align = 'CENTER' | 'LEFT' | 'RIGHT';

export type Field = {
  width: number;
  content: ReactElement | ReactElement[] | string;
  align: Align;
};

type GridRowProps = {
  fields: Field[];
};

export const GridRow = ({ fields }: GridRowProps): ReactElement => {
  return (
    <Row fields={fields}>
      {fields.map((field, index) => (
        <div key={index}>{field.content}</div>
      ))}
    </Row>
  );
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

type RowProps = GridRowProps;

const Row = styled.div<RowProps>(({ fields }) => {
  const template = fields.map(f => `${f.width}fr`).join(' ');
  return [
    css`
      display: grid;
      grid-template-columns: ${template};
      gap: 0.5rem;
      align-items: center;
    `,
  ];
});
