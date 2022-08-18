import React, { ReactElement } from 'react';
import tw, { css, styled } from 'twin.macro';

type Align = 'CENTER' | 'LEFT' | 'RIGHT';

export type Field = {
  width: number;
  content: ReactElement | ReactElement[] | string;
  align: Align;
};

type GridRowProps = {
  fields: Field[];
  minHeight?: number;
};

export const GridRow = ({ fields, minHeight }: GridRowProps): ReactElement => {
  return (
    <Row minHeight={minHeight} fields={fields}>
      {fields.map((field, index) => (
        <Field field={field} key={index}>
          {field.content}
        </Field>
      ))}
    </Row>
  );
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

type RowProps = GridRowProps;

const Row = styled.div<RowProps>(({ fields, minHeight }) => {
  const fullWidth = fields.reduce((p, c) => c.width + p, 0);
  const template = fields.map(f => `${(100 / fullWidth) * f.width}%`).join(' ');
  return [
    tw`max-w-full`,
    css`
      display: grid;
      grid-template-columns: ${template} fit-content(0%);
      gap: 0.5rem;
      align-items: center;
      min-height: ${minHeight || 0}rem;
    `,
  ];
});

type FieldProps = {
  field: Field;
};

const Field = styled.div<FieldProps>(({ field: { align } }) => {
  const alignGrid = mapAlign(align);
  return [
    tw`max-w-full`,
    css`
      text-align: ${align.toLowerCase()};
      justify-self: ${alignGrid};
    `,
  ];
});

type GridAlign = 'start' | 'end' | 'center';

const mapAlign = (align: Align): GridAlign => {
  switch (align) {
    case 'CENTER':
      return 'center';
    case 'LEFT':
      return 'start';
    case 'RIGHT':
      return 'end';
  }
};
