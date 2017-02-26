/// <reference path="../../typings/node.d.ts" />
import { match, Pattern, isAtom } from "../chapter3/match";
import { equal } from "../util";
import * as readline from 'readline';

interface X {
    s: (string | X)[];
}
type Proposition = any; // actually, the infinite type (string | Proposition)[];

export function prover() {
    const rl = readline.createInterface(process.stdin, process.stdout, undefined, /*terminal*/ true);
    console.log("Please enter proposition or HELP or RETURN.");
    (function rec() {
        rl.question("-->", answer => {
            if (answer.toLowerCase() === 'help') {
                console.log("Here's an example:");
                console.log("(a & (not b)) -> a");
            }
            if (answer.toLowerCase() === 'return') {
                return;
            }
            console.log(prove(parse(answer)));
            rec();
        });
    })();
}

export function prove(answer: Proposition): string {
    let result: Proposition;
    try {
        result = normalise(answer);
    }
    catch(e) {
        if (e.message === "syntax error") {
            result = [];
        }
    }
    if (valid([], [result])) {
        return "VALID";
    }
    else {
        return "INVALID";
    }
    //catch(e) {
        // if json exception, say so
        // if syntax error from reformat, then use its attached value (which will be null)
        // I think we are supposed to skip; I can't remember what catch does in common lisp
        //return "SYNTAX ERROR";
    //}
}

export function valid(l: Proposition, r: Proposition): boolean {
    // if any element is in both l and r, this proposition is valid
    for (const x of l) {
        if (r.some((y: any) => equal(x,y))) {
            return true;
        }
    }
    console.log(l);
    let m = match.call(helpers, [["*", 'x'], ['notWff', 'y'], ["*", 'z']], l);
    if (m) {
        return valid(m.x.concat(m.z), r.concat(m.y.slice(1)));
    }
    m = match.call(helpers, [["*", 'x'], ['notWff', 'y'], ["*", 'z']], r);
    if (m) {
        return valid(l.concat(m.y.slice(1)), m.x.concat(m.z));
    }
    m = match.call(helpers, [["*", 'x'], ['orWff', 'y'], ["*", 'z']], r);
    if (m) {
        return valid(l, m.x.concat([m.y[0]]).concat(m.y.slice(2)).concat(m.z));
    }
    m = match.call(helpers, [["*", 'x'], ['andWff', 'y'], ["*", 'z']], l);
    if (m) {
        return valid(m.x.concat([m.y[0]]).concat(m.y.slice(2)).concat(m.z), r);
    }
    m = match.call(helpers, [["*", 'x'], ['orWff', 'y'], ["*", 'z']], l);
    if (m) {
        return valid(m.x.concat([m.y[0]]), r) &&
            valid(m.x.concat(m.y.slice(2)), r);
    }
    m = match.call(helpers, [["*", 'x'], ['andWff', 'y'], ["*", 'z']], r);
    if (m) {
        return valid(l, m.x.concat([m.y[0]])) &&
            valid(l, m.x.concat(m.y.slice(2)));
    }
    return false;
}

export function normalise(x: Proposition): Proposition {
    if (isAtom(x)) {
        return x;
    }
    if (!wff(x)) {
        throw new Error("syntax error");
    }
    const [l, c, r] = x;
    if (notWff(x)) {
        return ['not', normalise(c)];
    }
    else if (c === 'implies') {
        return [['not', normalise(l)], 'or', normalise(r)];
    }
    else {
        return [normalise(l), c, normalise(r)];
    }
}
export function wff(x: Proposition): boolean {
    return isAtom(x) ||
        (x.length === 2 && x[0] === 'not' && wff(x[1])) ||
        (x.length === 3 && wff(x[0]) && isOp(x[1]) && wff(x[2]));
}
const notWff = (x: Proposition) => !isAtom(x) && x[0] === 'not';
const orWff = (x: Proposition) => !isAtom(x) && x[1] === 'or';
const andWff = (x: Proposition) => !isAtom(x) && x[1] === 'and';
const isOp = (x: string) => ['and', 'or', 'implies'].indexOf(x) > -1;

let helpers = { wff, notWff, orWff, andWff, isOp };

export function parse(s: string): Proposition {
    const tokens = tokenise(s);
    let i = 0;
    const e = parseExpression();
    if (i !== tokens.length) {
        throw new Error(`parse error: ${i} is less than ${tokens.length}`);
    }
    return Array.isArray(e) ? e : [e];

    function next(): string {
        const s = tokens[i];
        i++;
        return s;
    }

    function parseExpression(): Proposition {
        let e: Proposition;
        const token = next();
        if (token === 'not') {
            e = ['not', parseExpression()];
        }
        else if (token === '(') {
            e = parseExpression();
            if (next() !== ')') {
                throw new Error("expected matching )");
            }
        }
        else {
            e = token;
        }

        if (isOp(mapSynonym(tokens[i]))) {
            const l = e;
            const op = mapSynonym(next());
            if (!isOp(op)) {
                throw new Error(`unexpected operator ${op}`);
            }
            const r = parseExpression();
            return [l, op, r];
        }
        else {
            return e;
        }
    }

    function mapSynonym(op: string): string {
        switch(op) {
            case "&":
                return "and";
            case "|":
                return 'or';
            case "->":
            case "=>":
                return "implies";
            default:
                return op;
        }
    }
}
function tokenise(s: string): string[] {
    return s.split(/ |(\)|\()/).filter(id => id);
}
export function format(e: Proposition): string {
    if (isAtom(e)) {
        return e.toString();
    }
    if (e.length === 2 && e[0] === "not") {
        return "(not " + format(e[1]) + ")";
    }
    if (e.length === 3 && isOp(e[1])) {
        const l = format(e[0]);
        const r = format(e[2]);
        return `(${l} ${e[1]} ${r})`;
    }
    throw new Error(`tried to format malformed expression ${e}`);
}
