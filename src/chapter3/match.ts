import Cons = require("jscons");
export function isAtom(x: any) {
    return x === null || !Cons.instanceOf(x);
}
export function match2(p: any, s: any): boolean {
    if (isAtom(p)) {
        return isAtom(s);
    }
    else if (isAtom(s)) {
        return false;
    }
    else if (match2(p.head(), s.head())) {
        return match2(p.tail(), s.tail());
    }
    return false;
}
