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
function setDifference<T>(ts: T[], minus: T[]): T[] {
    const result: T[] = [];
    for (const t of ts) {
        if (minus.indexOf(t) === -1) {
            result.push(t);
        }
    }
    return result;
}
function extractPath(pointers: Map<string>, p: string): string[] {
    const path: string[] = [];
    while(p !== null) {
        path.push(p);
        p = pointers[p];
    }
    return path.reverse();
}
