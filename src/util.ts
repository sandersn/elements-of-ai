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
    }
    return true;
}
