import * as Cons from "jscons";
export function l(...xs: any[]): Cons {
    return Cons.from(xs);
}
export function equal(x: Cons | number | string | boolean, y: Cons | number | string | boolean): boolean {
    if (x === null && y === null) {
        return true;
    }
    else if (x === null || y === null) {
        return false;
    }
    else if (Cons.instanceOf(x) && Cons.instanceOf(y)) {
        return equal(x.head(), y.head()) && equal(x.tail(), y.tail());
    }
    else {
        return x === y;
    }
}
