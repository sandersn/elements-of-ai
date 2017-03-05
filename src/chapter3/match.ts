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
export function match4(p: any[], s: any[]): Map<any> | undefined {
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
        return false;
    }
}
export function match5(this: any, p: any[], s: any[]): Map<any> | undefined {
    if (p.length !== s.length) return;
    const d: Map<any> = {};
    const matchHelper = (x: any[], i: number) => {
        if (equal(x, s[i])) {
            return true;
        }
        else if (x.length === 2 &&
                 (x[0] === "?" ||
                  (this && this[x[0]] && this[x[0]](s[i], d)))) {
                d[x[1]] = s[i];
            return true;
        }
        return false;
    }
    if (p.every(matchHelper)) {
        return d;
    }
}
export type Literal = string | number;
export type Variable = [string, string];
export type Pattern = (Literal | Variable)[]
function isLiteral(x: any): x is Variable {
    return Array.isArray(x) && x.length === 2 && typeof x[0] === "string" && typeof x[1] === "string";
}
export function match(this: Map<(v: any, d:Map<any>) => boolean> | void, p: Pattern, s: any[]): Map<any> | undefined {
    if (!Array.isArray(p) || !Array.isArray(s)) {
        return;
    }
    if (p.length === 0) {
        return s.length === 0 ? {} : undefined;
    }

    const d: Map<any> = {};
    let offset: number = 0;
    const matchHelper = (x: Literal | Variable, i: number) => {
        const si = i + offset;
        if (equal(x, s[si])) {
            return true;
        }
        if (!isLiteral(x)) {
            return false;
        }
        if (x[0] === "?" || (this && this[x[0]] && this[x[0]](s[si], d))) {
            d[x[1]] = s[si];
            return true;
        }
        else if (x[0] === "*") {
            if (i + 1 === p.length) {
                d[x[1]] = s.slice(si);
                return true;
            }
            for(var j = 0; si + j < s.length; j++) {
                if (match.call(this, [p[i + 1]], [s[si + j]])) {
                    break;
                }
            }
            d[x[1]] = s.slice(si, si + j);
            offset += j - 1;
            return true;
        }
        return false;
    }
    if (p.every(matchHelper)) {
        return d;
    }
}
