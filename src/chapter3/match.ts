import { equal, Map } from "../util";
export function isAtom(x: any) {
    return !Array.isArray(x) || x.length === 0;
}
export function isEmpty(x: any) {
    return Array.isArray(x) && x.length === 0;
}
export function match2(p: any, s: any): boolean {
    if (isAtom(p)) {
        return isAtom(s);
    }
    else if (isAtom(s)) {
        return false;
    }
    else if (match2(p[0], s[0])) {
        return match2(p.slice(1), s.slice(1));
    }
    return false;
}
export function match3(p: any[], s: any[]): boolean {
    if (p.length !== s.length) return false;
    return p.every((x,i) => x === "?" || equal(x, s[i]));
}
export function match4(p: any[], s: any[]): Map<any> {
    if (p.length !== s.length) return;
    let d: Map<any> = {};
    if (p.every(matchHelper)) {
        return d;
    }
    function matchHelper(x: any, i: number) {
        if (equal(x, s[i])) {
            return true;
        }
        else if (x.length === 2 && x[0] === "?") {
            d[x[1]] = s[i];
            return true;
        }
    }
}

