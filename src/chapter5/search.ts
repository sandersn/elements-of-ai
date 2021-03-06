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
export const franceDistance: Map<[string,number][]> = {
    brest: [['rennes', 244]],
    rennes: [['caen', 176], ['paris', 348], ['brest', 244], ['nantes', 107]],
    caen: [['calais', 120], ['paris', 241], ['rennes', 176]],
    calais: [['nancy', 534], ['paris', 297], ['caen', 120]],
    nancy: [['strasbourg', 145], ['dijon', 201], ['paris', 372], ['calais', 534]],
    strasbourg: [['dijon', 335], ['nancy', 145]],
    dijon: [['strasbourg', 335], ['lyon', 192], ['paris', 313], ['nancy', 201]],
    lyon: [['grenoble', 104], ['avignon', 216], ['limoges', 389], ['dijon', 192]],
    grenoble: [['avignon', 227], ['lyon', 104]],
    avignon: [['grenoble', 227], ['marseille', 99], ['montpellier', 121], ['lyon', 216]],
    marseille: [['nice', 188], ['avignon', 99]],
    nice: [['marseille', 188]],
    montpellier: [['avignon', 121], ['toulouse', 240]],
    toulouse: [['montpellier', 240], ['bordeaux', 253], ['limoges', 313]],
    bordeaux: [['limoges', 220], ['toulouse', 253], ['nantes', 330]],
    limoges: [['lyon', 389], ['toulouse', 313], ['bordeaux', 220], ['nantes', 329], ['paris', 396]],
    nantes: [['limoges', 329], ['bordeaux', 330], ['rennes', 107]],
    paris: [['calais', 297], ['nancy', 372], ['dijon', 313], ['limoges', 396], ['rennes', 348], ['caen', 241]]
}
export function depthFirstSearch(graph: Map<string[]>, start: string, goal: string): string[] | null {
    let open = [start];
    let pointers: Map<string | null> = {};
    let closed: string[] = [];
    let n: string;
    let l: string[];
    let openCount = 0;
    pointers[start] = null;
    while (open.length) {
        n = open.pop()!;
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
export function breadthFirstSearch(graph: Map<string[]>, start: string, goal: string): [string[], number] | null {
    let open = [start];
    let pointers: Map<string | null> = {};
    let closed: string[] = [];
    let n: string;
    let l: string[];
    let openCount = 0;
    pointers[start] = null;
    while (open.length) {
        n = open.pop()!;
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
export function bestFirstSearch(graph: Map<string[]>, f: (s: string) => number, start: string, goal: string): [string[], number] | null {
    const pointers: Map<string | null> = {};
    const values: Map<number> = {};
    let open = [start];
    const closed: string[] = [];
    pointers[start] = null;
    values[start] = f(start);
    let openCount = 0;
    while (open.length) {
        const n = open.pop()!;
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
export type Distance = number & { _brand1: any };
export type Intercity = number & { _brand2: any };
export function uniformCost(graph: Map<[string, Intercity][]>, start: string, goal: string): [string[], number] | null {
    const pointers: Map<string | null> = {};
    const values: Map<Distance> = {};
    let open: string[] = [start];
    const closed: string[] = [];
    pointers[start] = null;
    values[start] = 0 as Distance;
    let openCount = 1;
    while (open.length) {
        const n = open.pop()!;
        closed.push(n);
        if (n === goal) {
            return [extractPath(pointers, n), openCount];
        }
        const l = graph[n];
        for (const remaining of setDifferenceFirst(l, closed)) {
            const [m, dst] = remaining;
            const temp = (values[n] + dst) as Distance;
            if (open.indexOf(m) > -1) {
                if (temp < values[m]) {
                    values[m] = temp;
                    pointers[m] = n;
                    openCount++;
                }
            }
            else {
                values[m] = temp;
                pointers[m] = n;
                open.push(m);
                openCount++;
            }
        }
        open.sort((s1,s2) => values[s1] === values[s2] ? 0
                           : values[s1] < values[s2] ? 1
                           : -1);
    }
    return null;
}
function fst<T,U>([x,y]: [T, U]): T {
    return x;
}
function snd<T,U>([x,y]: [T, U]): U {
    return y;
}
function arcDist(n2: string, l: [string, number][]): number {
    return cdrSelect(n2, l);
}
function cdrSelect(key: string, l: [string, number][]): number {
    for (const [s, n] of l) {
        if (key === s) {
            return n;
        }
    }
    return Number.MAX_VALUE;
}
function setDifferenceFirst<T,U>(ts: [T,U][], minus: T[]): [T,U][] {
    const result: [T,U][] = [];
    for (const [t,u] of ts) {
        if (minus.indexOf(t) === -1) {
            result.push([t,u]);
        }
    }
    return result;
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
function extractPath(pointers: Map<string | null>, p: string | null): string[] {
    const path: string[] = [];
    while(p !== null) {
        path.push(p);
        p = pointers[p];
    }
    return path.reverse();
}
export function longitudeEstimate(n: string, goal: string): Distance {
    return 10 * longitudeDifference(n, goal) as Distance;
}
function bestByList<T>(l: T[], f: (t: T) => number): T[] {
    let bestValues: T[] = [l[0]];
    let best = f(l[0]);
    for (const x of l.slice(1)) {
        const key = f(x);
        if (key < best) {
            bestValues = [x];
        }
        else if (key === best) {
            bestValues.push(x);
        }
    }
    return bestValues;
}
function find<T>(l: T[], f: (t: T) => boolean): T | undefined {
    if (l == null) throw new Error("find(l) is undefined");
    for (const x of l) {
        if (f(x)) {
            return x;
        }
    }
}
function remove<T>(l: T[], x: T): void {
    const i = l.indexOf(x);
    if (i > -1) {
        l.splice(i, 1);
    }
}
function moveNext(open: string[], estimatedTotal: Map<Distance>, goal: string) {
    const l = bestByList(open, s => estimatedTotal[s]);
    return l.length === 1 ? l[0] : (find(l, x => x === goal) || l[0]);
}
export function aStar(graph: Map<[string, Intercity][]>, f: (n: string, goal: string) => Distance, start: string, goal: string): [string[], number] | null {
    const predecessors: Map<string | null> = {};
    const distance: Map<Distance> = {};
    const estimatedTotal: Map<Distance> = {};
    let open: string[] = [start];
    const closed: string[] = [];
    predecessors[start] = null;
    distance[start] = 0 as Distance;
    estimatedTotal[start] = f(start, goal);

    let found = false;
    let openCount = 1;
    // TODO: It would probably be better to order open by f every time its contents change
    // (or f's value changes)
    while (open.length && !found) {
        const current = moveNext(open, estimatedTotal, goal);
        remove(open, current);
        closed.push(current);
        if (current === goal) {
            found = true;
            return [extractPath(predecessors, current), openCount];
        }
        const successors = graph[current];
        for (const [successor, dst] of successors) {
            if (open.indexOf(successor) === -1 && closed.indexOf(successor) === -1) {
                distance[successor] = distance[current] + dst as Distance;
                estimatedTotal[successor] = distance[successor] + f(successor, goal) as Distance;
                predecessors[successor] = current;
                open.push(successor);
                openCount++;
            }
            else {
                const previousParent = predecessors[successor];
                const zy = previousParent ? find(graph[previousParent], ([edge, _]) => edge === successor) : ["start", 0] as [string, Intercity];
                const previousDistance = previousParent ? distance[previousParent] : 0 as Distance;
                if (!zy) throw new Error("could not find current node in previous node's edges");
                const [_, alternateIntercity] = zy;
                const alternateEstimate = estimatedTotal[successor] - previousDistance - alternateIntercity + distance[current] + dst;
                if (alternateEstimate < estimatedTotal[successor]) {
                    distance[successor] = distance[successor] + alternateEstimate - estimatedTotal[successor] as Distance;
                    estimatedTotal[successor] = alternateEstimate as Distance;
                    predecessors[successor] = current;
                    if (closed.indexOf(successor) > -1) {
                        open.push(successor);
                        remove(closed, successor);
                    }
                }
            }
        }
    }
    return null;
}
