import { match } from "../chapter3/match";
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
