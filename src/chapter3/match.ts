import Cons = require("jscons");
import { equal } from "../util";
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
export function match3(p: Cons, s: Cons): boolean {
    if (p === null) {
        return s === null;
    }
    else if (isAtom(p) || isAtom(s)) {
        return false;
    }
    else if (equal(p.head(), s.head())) {
        return match3(p.tail(), s.tail());
    }
    else if (p.head() === "?") {
        return match3(p.tail(), s.tail());
    }
    return false;
}
