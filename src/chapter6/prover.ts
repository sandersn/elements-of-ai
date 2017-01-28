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
                console.log("[['a', 'and', ['not', 'b']], 'implies', 'a']");
            }
            if (answer.toLowerCase() === 'return') {
                return;
            }
            try {
                const result = reformat(JSON.parse(answer));
                if (valid(null, [result])) {
                    console.log("VALID");
                }
                else {
                    console.log("INVALID");
                }
            }
            catch(e) {
                // if json exception, say so
                // if syntax error from reformat, then use its attached value (which will be null)
                // I think we are supposed to skip; I can't remember what catch does in common lisp
                console.log("SYNTAX ERROR");
            }
            rec();
        });
    })();
}

function valid(l: Proposition, r: Proposition): boolean {
    // if any element is in both l and r, this proposition is valid
    for (const x of l) {
        if (r.some((y: any) => equal(x,y))) {
            return true;
        }
    }
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

function reformat(x: Proposition): Proposition {
    if (isAtom(x)) {
        return x;
    }
    if (!wff(x)) {
        throw new Error("syntax error");
    }
    const [l, c, r] = x;
    if (notWff(x)) {
        return ['not', reformat(c)];
    }
    else if (c === 'implies') {
        return [['not', reformat(l)], 'or', reformat(r)];
    }
    else {
        return [reformat(l), c, reformat(r)];
    }
}
function wff(x: Proposition): boolean {
    return isAtom(x) ||
        (x.length === 2 && x[0] === 'not' && wff(x[1])) ||
        (x.length === 3 && wff(x[0]) && op(x[1]) && wff(x[2]));
}
const notWff = (x: Proposition) => !isAtom(x) && x[0] === 'not';
const orWff = (x: Proposition) => !isAtom(x) && x[1] === 'or';
const andWff = (x: Proposition) => !isAtom(x) && x[1] === 'and';
const op = (x: string) => ['and', 'or', 'implies'].indexOf(x) > -1;

let helpers = { wff, notWff, orWff, andWff, op };
