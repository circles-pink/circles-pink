import tw, { styled } from 'twin.macro';

export const Input = tw.input`shadow appearance-none border border-pink-600 rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight w-full`;
export const ButtonGray = tw.button`bg-gray-600 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded-full mr-1 cursor-pointer`;
export const ButtonPink = tw.button`bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full mr-1 cursor-pointer`;

type StyledInputProps = {
  indicatorColor: string;
};

export const InputWithProps = styled.input<StyledInputProps>`
  border: 1px solid ${props => props.indicatorColor};
  border-radius: 0.25rem;
  width: 100%;
  padding-top: 0.5rem;
  padding-bottom: 0.5rem;
  padding-left: 0.75rem;
  padding-right: 0.75rem;
  margin-bottom: 0.75rem;
  line-height: 1.25;
  outline: none;
`;
