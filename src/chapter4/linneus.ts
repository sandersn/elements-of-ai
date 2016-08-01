import { match, Pattern } from "../chapter3/match";
import { Map } from "../util";
export function addToList<T>(k: string, v: T, d: Map<T[]>) {
    if (d[k]) {
        d[k].push(v);
    }
    else {
        d[k] = [v];
    }
}
export function isaTest(isa: Map<string[]>, x: string, y: string, n: number): boolean {
    if (n === 0) return false;
    if (x === y) return true;
    return isa[x] && (isa[x].indexOf(y) > -1 ||
                      isa[x].some(s => isaTest(isa, s, y, n - 1)));
}
const articles: Map<boolean> = { 'a': true,
                                 'an': true,
                                 'the': true,
                                 'that': true,
                                 'this': true,
                                 'those': true,
                                 'these': true };
export function isArticle(article: string) {
    return articles[article];
}
type RetVal = [string, any, any, any];
function caseMatch(text: any[], cases: [Pattern[], (...args:any[]) => RetVal][]): RetVal {
    for (const [patterns, f] of cases) {
        const d = patterns.some(p => !!match(p, text));
        if (d) {
            return f(d)
        }
    }
    console.log("I do not understand");
    return ["error", {}, {}, {}];
}
function chainInterpret(utterances: any[]) {
    let ds: RetVal = ["start", {}, {}, {}];
    let interpret: Function;
    for (const utterance in utterances) {
        // TODO: Should be a reduce
        ds = interpret.apply(undefined, utterance, ds.slice(1));
    }
    return ds;
}
export function makeConj(l: string[], article: Map<string>) {
    return l.map(x => `${article[x]} ${x}`).join(" and ");
}
function tell(article: Map<string>, x: string, y: string) {
    return `${article[x]} ${x} is ${article[y]} ${y}`;
}
function explainChain(isa: Map<string[]>, article: Map<string>, x: string, l: string[], y: string): string {
    if (l === []) {
        return '';
    }
    else if (l.indexOf(y) > -1) {
        return " and " + tell(article, x, y);
    }
    else if (isaTest(isa, l[0], y, 10)) {
        return tell(article, x, l[0]) + explainChain(isa, article, l[0], isa[l[0]], y);
    }
    else {
        return explainChain(isa, article, x, l.slice(1), y);
    }
}
