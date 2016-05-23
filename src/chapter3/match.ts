import { equal } from "../util";
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
    if (isEmpty(p)) {
        return isEmpty(s);
    }
    else if (isAtom(p) || isAtom(s)) {
        return false;
    }
    else if (equal(p[0], s[0])) {
        return match3(p.slice(1), s.slice(1));
    }
    else if (p[0] === "?") {
        return match3(p.slice(1), s.slice(1));
    }
    return false;
}
