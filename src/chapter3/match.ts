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
export function match5(p: any[], s: any[]): Map<any> {
    if (p.length !== s.length) return;
    const d: Map<any> = {};
    const matchHelper = (x: any, i: number) => {
        if (equal(x, s[i])) {
            return true;
        }
        else if (x.length === 2 &&
                 (x[0] === "?" ||
                  (this && this[x[0]] && this[x[0]](s[i])))) {
                d[x[1]] = s[i];
            return true;
        }
    }
    if (p.every(matchHelper)) {
        return d;
    }
}
export function match(p: any[], s: any[]): Map<any> {
    if (!Array.isArray(p) || !Array.isArray(s)) {
        return;
    }
    if (p.length === 0) {
        return s.length === 0 ? {} : undefined;
    }

    const d: Map<any> = {};
    let offset: number = 0;
    const matchHelper = (x: any, i: number) => {
        if (equal(x, s[i + offset])) {
            return true;
        }
        else if (x.length === 2 &&
                 (x[0] === "?" ||
                  (this && this[x[0]] && this[x[0]](s[i + offset])))) {
                d[x[1]] = s[i + offset];
            return true;
        }
        else if (x.length === 2 &&
                 x[0] === "*") {
            if (i + 1 === p.length) {
                d[x[1]] = s.slice(i + offset);
                return true;
            }
            for(var j = 1; i + offset + j < s.length; j++) {
                if (match([p[i + 1]], [s[i + offset + j]])) {
                    break;
                }
            }
            d[x[1]] = s.slice(i + offset, i + offset + j);
            offset += j - 1;
            return true;
        }
    }
    if (p.every(matchHelper)) {
        return d;
    }
}
