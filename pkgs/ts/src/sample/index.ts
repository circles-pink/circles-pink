import * as PursAPI from "../../../../generated"

export const myFn = () => {

    const result = PursAPI.myApi(6)("Hello1")

    console.log("hello from ps:", result, PursAPI.gravity, PursAPI.name)
    console.log(PursAPI)


}