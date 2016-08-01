import { match, Pattern } from "../chapter3/match";
import { Map, findKey } from "../util";
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
type RetVal = [string, Map<string[]>, Map<string[]>, Map<string>];
function caseMatch(text: string[], cases: [Pattern[], (d:Map<any>) => string][]): string {
    for (const [patterns, f] of cases) {
        const d = findKey(patterns, p => match(p, text));
        if (d) {
            return f(d);
        }
    }
    console.log("I do not understand");
    return "error";
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
export function explainLinks(isa: Map<string[]>, article: Map<string>, x: string, y: string) {
    if (x === y) {
        return "They are identical";
    }
    else if (isa[x].indexOf(y) > -1) {
        return "You told me";
    }
    else {
        return explainChain(isa, article, x, isa[x], y);
    }
}
function interpret(text: string[], isa: Map<string[]>, includes: Map<string[]>, article: Map<string>): string {
    return caseMatch(text, [
        [[[['isArticle', 'article1'], ["?", 'x'], 'is', ['isArticle', 'article2'], ["?", 'y']]],
         d => {
             console.log("I understand.");
             addToList(d['x'], d['y'], isa);
             addToList(d['y'], d['x'], includes);
             article[d['x']] = d['article1'];
             article[d['y']] = d['article2'];
             return "add-fact";
         }],
        [[['what', 'is', ["?", 'x']],
          ['what', 'is', ["isArticle", 'article1'], ["?", 'x']]],
         d => {
             const y = isa[d['x']] || includes[d['x']];
             const flag = isa[d['x']] ? 'isa' : includes[d['x']] ? 'includes' : 'dunno';
             if (flag === 'dunno') {
                 console.log("I don't know.");
             }
             else {
                 const verb = flag === 'isa' ? 'is' : 'is something more general than';
                 console.log(article[d['x']], d['x'], verb, makeConj(y, article));
             }
             return flag;
         }],
        [[['is', ['isArticle', 'article1'], ["?", 'x'], ['isArticle', 'article2'], ["?", 'y']]],
         d => {
             if (isaTest(isa, d['x'], d['y'], 10)) {
                 console.log("Yes indeed", article['x'], d['x'], 'is', article['y'], d['y']);
                 return 'is-indeed';
             }
             else {
                 console.log("Sorry, not that I know of.");
                 return 'is-not';
             }
         }],
        [[['why', 'is', ['isArticle', 'article1'], ["?", 'x'], ['isArticle', 'article2'], ["?", 'y']]],
         d => {
             if (isaTest(isa, d['x'], d['y'], 10)) {
                 console.log("Because", explainLinks(isa, article, d['x'], d['y']));
                 return 'because';
             }
             else {
                 console.log("But it's not!");
                 return 'its-not';
             }
         }],
        [[['bye'], ['goodbye']],
         d => 'bye']
    ]);
}
/** Run a whole bunch of statements, for testing purposes */
export function chainInterpret(utterances: string[]) {
    const isa: Map<string[]> = {};
    const includes: Map<string[]> = {};
    const article: Map<string> = {};
    let state = 'start';
    for (const utterance in utterances) {
        // TODO: Should be a reduce
        state = interpret(utterance.split(' '), isa, includes, article);
    }
    return [state, isa, includes, article];
}
export function linneus() {
    // TODO: I have no idea how to read from console in node!
    let readLine: () => string;
    console.log("This is Linneus. Please talk to me.");
    const isa: Map<string[]> = {};
    const includes: Map<string[]> = {};
    const article: Map<string> = {};
    while(true) {
        console.log("-->");
        if (interpret(readLine().split(' '), isa, includes, article) === 'bye') {
            console.log("Goodbye");
            return;
        }
    }
}
