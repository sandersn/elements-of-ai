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
    calais: 18,
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
export function breadthFirstSearch(graph: Map<string[]>, start: string, goal: string): [string[], number] {
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
            return [extractPath(pointers, n), openCount];
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
export function bestFirstSearch(graph: Map<string[]>, f: (s: string) => number, start: string, goal: string): [string[], number] {
    const pointers: Map<string> = {};
    const values: Map<number> = {};
    let open = [start];
    const closed: string[] = [];
    pointers[start] = null;
    values[start] = f(start);
    let openCount = 0;
    while (open.length) {
        const n = open.pop();
        closed.push(n);
        if (n === goal) {
            return [extractPath(pointers, n), openCount];
        }
        const l = graph[n];
        const news = setDifference(setDifference(l, open), closed);
        openCount += news.length;
        for (const j of news) {
            values[j] = f(j);
            pointers[j] = n;
        }
        for (const j of intersection(l, open)) {
            values[j] = Math.min(f(j), values[j]);
        }
        open = open.concat(news);
        open.sort((s1,s2) => values[s1] === values[s2] ? 0
                           : values[s1] < values[s2] ? 1
                           : -1);
    }
    return null;
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
    for (const t of ts) {
        if (add.indexOf(t) > -1) {
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
