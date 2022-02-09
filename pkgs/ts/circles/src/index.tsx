import tw from 'twin.macro'

const Button = tw.button`border hover:border-black`

export const DemoComponent = () => {
    return <Button>Hello!</Button>
}