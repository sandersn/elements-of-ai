import Cons = require("jscons");
export function isAtom(x: any) {
    return !Cons.instanceOf(x);
}

