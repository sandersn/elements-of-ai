import { Map } from '../util';
export const france: Map<string[]> = {
    brest: ['rennes'],
    rennes: ['caen', 'paris', 'brest', 'nantes'],
    caen: ['calais', 'paris', 'rennes'],
    calais: ['nancy', 'paris', 'caen'],
    nancy: ['strasbourg', 'dijon', 'paris', 'calais'],
    strasbourg: ['dijon', 'nancy'],
    dijon: ['strasbourg', 'lyon', 'paris', 'nancy'],
    lyon: ['grenoble', 'avignon', 'limoges', 'dijon'],
    grenoble: ['avignon', 'lyon'],
    avignon: ['grenoble', 'marseille', 'montpellier', 'lyon'],
    marseille: ['nice', 'avignon'],
    nice: ['marseille'],
    montpellier: ['avignon', 'toulouse'],
    toulouse: ['montpellier', 'bordeaux', 'limoges'],
    bordeaux: ['limoges', 'toulouse', 'nantes'],
    limoges: ['lyon', 'toulouse', 'bordeaux', 'nantes', 'paris'],
    nantes: ['limoges', 'bordeaux', 'rennes'],
    paris: ['calais', 'nancy', 'dijon', 'limoges', 'rennes', 'caen']
}
export const longitude: Map<number> = {
    avignon: 48,
    bordeaux: -6,
    brest: -45,
    caen: -4,
    calias: 18,
    dijon: 51,
    grenoble: 57,
    limoges: 12,
    lyon: 48,
    marseille: 53,
    montpellier: 36,
    nantes: -16,
    nancy: 62,
    nice: 73,
    paris: 23,
    rennes: -17,
    strasbourg: 77,
    toulouse: 14
}
export function depthFirstSearch(graph: Map<string[]>, start: string, goal: string): string[] {
    let open = [start];
    let pointers: Map<string> = {};
    let closed: string[] = [];
    let n: string;
    let l: string[];
    let openCount = 0;
    pointers[start] = null;
    while (open.length) {
        n = open.pop();
        openCount++;
        closed.push(n);
        if (n === goal) {
            return extractPath(pointers, n);
        }
        // reversed to match book's Lisp implementation;
        // it's not actually required
        l = setDifference(graph[n as string], closed).reverse();
        open = setDifference(open, l).concat(l);
        for (const x of l) {
            pointers[x] = n;
        }
    }
    return null;
}
export function breadthFirstSearch(graph: Map<string[]>, start: string, goal: string): string[] {
    let open = [start];
    let pointers: Map<string> = {};
    let closed: string[] = [];
    let n: string;
    let l: string[];
    let openCount = 0;
    pointers[start] = null;
    while (open.length) {
        n = open.pop();
        openCount++;
        closed.push(n);
        if (n === goal) {
            console.log(openCount);
            return extractPath(pointers, n);
        }
        // reversed to match book's Lisp implementation;
        // it's not actually required
        l = setDifference(setDifference(graph[n as string], open), closed).reverse();
        open = l.concat(open);
        for (const x of l) {
            pointers[x] = n;
        }
    }
    return null;
}
export function bestFirstSearch(graph: Map<string[]>, f: (s: string) => number, start: string, goal: string): string[] {
    let pointers: Map<string> = {};
    let values: Map<number> = {};
    let open = [start];
    pointers[start] = null;
    values[start] = f(start);
    let closed: string[] = [];
    let n: string;
    let l: string[];
    let val: number;
    let openCount = 0;
    while (open.length) {
        n = removeBest(open, goal, values);
        closed.push(n);
        if (n === goal) {
            console.log(openCount);
            return extractPath(pointers, n);
        }
        l = setDifference(graph[n], closed);
        for (const j of setDifference(setDifference(l, open), closed)) {
            // TODO: This should be replaced with calls to in-place sort I think
            openCount++;
            // TODO: Might need -f(j) to get same sorting as Lisp version
            const val = f(j);
            values[j] = val;
            open = insert(open, j, val, values);
            pointers[j] = n;
        }
        for (const j of intersection(l, open)) {
            const val = f(j);
            if (val < values[j]) {
                values[j] = val;
                open = insert(remove(open, j), j, val, values);
            }
        }
    }
    return null;
}
function removeBest(l: string[], goal: string, values: Map<number>): string {
    if (l[0] === goal) {
        return l[0];
    }
    else {
        return better(l[0], l.slice(1), goal, values);
    }
}
function better(x: string, l: string[], goal: string, values: Map<number>): string {
    if (l.length === 0) {
        return x;
    }
    else if (values[x] < values[l[0]]) {
        return x;
    }
    else if (l[0] === goal) {
        return l[0];
    }
    else {
        return better(x, l.slice(1), goal, values);
    }
}
function remove<T>(ts: T[], target: T): T[] {
    const i = ts.indexOf(target);
    if (i === -1) {
        return ts;
    }
    else {
        return ts.slice(0, i).concat(ts.slice(i + 1));
    }
}
function insert(l: string[], node: string, value: number, values: Map<number>): string[] {
    if (!l.length) {
        return [node];
    }
    if (value < values[l[0]]) {
        return [node].concat(l);
    }
    else {
        return [l[0]].concat(insert(l.slice(1), node, value, values));
    }
}
function setDifference<T>(ts: T[], minus: T[]): T[] {
    const result: T[] = [];
    for (const t of ts) {
        if (minus.indexOf(t) === -1) {
            result.push(t);
        }
    }
    return result;
}
function intersection<T>(ts: T[], add: T[]): T[] {
    const result: T[] = ts.slice();
    for (const t of add) {
        if (ts.indexOf(t) === -1) {
            result.push(t);
        }
    }
    return result;
}
export function longitudeDifference(n1: string, n2: string): number {
    return Math.abs(longitude[n1] - longitude[n2]);
}
function extractPath(pointers: Map<string>, p: string): string[] {
    const path: string[] = [];
    while(p !== null) {
        path.push(p);
        p = pointers[p];
    }
    return path.reverse();
}
