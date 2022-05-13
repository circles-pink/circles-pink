import { css, styled } from 'twin.macro';

type StyledInputProps = {
  indicatorColor?: string;
};

export const Input = styled.input<StyledInputProps>(props => [
  css`
    border: 1px solid ${props.indicatorColor || 'black'};
    border-radius: 0.25rem;
    width: 100%;
    padding-top: 0.5rem;
    padding-bottom: 0.5rem;
    padding-left: 0.75rem;
    padding-right: 0.75rem;
    margin-bottom: 0.75rem;
    line-height: 1.25;
    outline: none;
    font-size: 1rem;
  `,
]);
