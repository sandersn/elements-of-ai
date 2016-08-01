export interface Map<T> {
    [s: string]: T;
}
export function equal(x: any[] | number | string | boolean, y: any[] | number | string | boolean): boolean {
    if (x === y) return true;
    else if (x == null || y == null) return false;
    else if (Array.isArray(x) && Array.isArray(y)) {
        if (x.length !== y.length) return false;
        for(var i = 0; i < x.length; i++) {
            if (!equal(x[i], y[i])) {
                return false;
            }
        }
        return true;
    }
    return false;
}
export function chooseRandom<T>(l: T[]): T {
    return l[Math.floor(Math.random() * l.length)];
}
export function findKey<T, U>(l: T[], f: (t: T) => U): U {
    for (const x of l) {
        const key = f(x);
        if (key) {
            return key;
        }
    }
}
