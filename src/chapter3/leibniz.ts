import { Pattern, Literal, isAtom, match } from "./match";
import { Map, equal } from "../util";
/// types
interface Rule {
    pattern: any;
    action: (d: State) => any;
    name: string;
}
type State = Map<any>;
type Expression = any[];
/// rules ///
export function existsTree(tree: any[], x: any): boolean {
    if (isAtom(tree)) {
        return tree === x;
    }
    else {
        return tree.some(child =>
                         isAtom(child) ? child === x : existsTree(child, x));
    }
}
export function rewriteTreeSingle(tree: Expression, f: (x: any) => any): Expression {
    function update(tree: Expression, i: number, x: any) {
        const copy = tree.slice();
        copy[i] = x;
        return copy;
    }
    function recur(tree: Expression): Expression {
        const topLevel = f(tree);
        if (topLevel !== undefined || !Array.isArray(tree)) {
            return topLevel;
        }
        for(let i = 0; i < tree.length; i++) {
            const result = f(tree[i]);
            if (result !== undefined) {
                return update(tree, i, result);
            }
            else {
                const recursiveResult = recur(tree[i]);
                if (recursiveResult !== undefined) {
                    return update(tree, i, recursiveResult);
                }
            }
        }
    }
    return recur(tree);
}
function binaryDiffHelper(pattern: Pattern): (v: any, d: State) => boolean {
    return (v, d) => {
        if (isAtom(v)) return false;
        const d2 = match.call(helpers, pattern, v);
        if (d2) {
            for (const k of Object.keys(d2)) {
                d[k] = d2[k];
            }
            return true;
        }
    }
}
function isNumber(x: any): x is number {
    return typeof x === 'number';
}
const helpers: Map<(v: any, d:State) => boolean> = {
    isNestedDiffSum: binaryDiffHelper(["+", ["?", 'e1'], ["?", 'e2']]),
    isNestedDiffX1: (v, d) => true,
    isNestedDiffX2: (v, d) => equal(v, d['e1']),
    diffConstE2Rule: (v, d) => !existsTree(d['e1'], v),
    diffProductE3Rule: binaryDiffHelper(["*", ["?", 'e1'], ["?", 'e2']]),
    diffPowerE3Rule: binaryDiffHelper(["exp", ["?", 'e1'], ['isNumber', 'e2']]),
    isNumber: (v, d) => isNumber(v)
}
const diffSumRule: Rule = {
    pattern: ["d", ["isNestedDiffSum", "e3"], ["?", "v1"]],
    action: d => ["+", ['d', d['e1'], d['v1']], ['d', d["e2"], d["v1"]]],
    name: "diffSumRule"
};
const diffXRule: Rule = {
    pattern: ["d", ["isNestedDiffX1", "e1"], ["isNestedDiffX2", "e2"]],
    action: d => 1,
    name: "diffXRule"
}
const diffConstRule: Rule = {
    pattern: ["d", ["?", 'e1'], ["diffConstE2Rule", "e2"]],
    action: d => 0,
    name: "diffConstRule"
}
const diffProductRule: Rule = {
    pattern: ["d", ["diffProductE3Rule", "e3"], ["?", 'v1']],
    action: d => ["+",
                  ["*", d['e2'], ['d', d['e1'], d['v1']]],
                  ["*", d['e1'], ['d', d['e2'], d['v1']]]],
    name: "diffProductRule"
}
const diffPowerRule: Rule = {
    pattern: ['d', ["diffPowerE3Rule", 'e3'], ["?", 'v1']],
    action: d => ["*",
                  d['e2'],
                  ["*",
                   ["exp", d['e1'], ['dec', d['e2']]],
                   ['d', d['e1'], d['v1']]]],
    name: "diffPowerRule"
}
const laddZeroRule: Rule = {
    pattern: ["+", ["?", 'x'], 0],
    action: d => d['x'],
    name: "laddZeroRule"
}
const raddZeroRule: Rule = {
    pattern: ["+", 0, ["?", 'x']],
    action: d => d['x'],
    name: "raddZeroRule"
}
const addConstRule: Rule = {
    pattern: ["+", ["isNumber", 'x'], ["isNumber", "y"]],
    action: d => d['x'] + d['y'],
    name: "addConstRule"
}
const lprodZeroRule: Rule = {
    pattern: ["*", ["?", 'x'], 0],
    action: d => 0,
    name: "lprodZeroRule"
}
const rprodZeroRule: Rule = {
    pattern: ["*", 0, ["?", 'x']],
    action: d => 0,
    name: "rprodZeroRule"
}
const lprodIdRule: Rule = {
    pattern: ["*", ["?", 'x'], 1],
    action: d => d['x'],
    name: "lprodIdRule"
}
const rprodIdRule: Rule = {
    pattern: ["*", 1, ["?", 'x']],
    action: d => d['x'],
    name: "rprodIdRule"
}
const prodConstRule: Rule = {
    pattern: ["*", ["isNumber", 'x'], ["isNumber", "y"]],
    action: d => d['x'] * d['y'],
    name: "prodConstRule"
}
const expZeroRule: Rule = {
    pattern: ["exp", ["?", 'x'], 0],
    action: d => 1,
    name: "expZeroRule"
}
const expIdRule: Rule = {
    pattern: ["exp", ["?", 'x'], 1],
    action: d => d['x'],
    name: "expIdRule"
}
const decRule: Rule = {
    pattern: ["dec", ["isNumber", 'x']],
    action: d => d['x'] - 1,
    name: "decRule"
}
                  
export const rules: Rule[][] = [
    [diffSumRule, diffXRule, diffConstRule, diffProductRule, diffPowerRule],
    [laddZeroRule, raddZeroRule, addConstRule, lprodZeroRule, rprodZeroRule, lprodIdRule, rprodIdRule, prodConstRule, expZeroRule, expIdRule, decRule]]

/// control scheme ///
function fire(rule: Rule, d: State) {
    return rule.action(d);
}
export function tryRule(expression: Expression, rule: Rule): Expression {
    if (isAtom(expression)) return undefined;
    return rewriteTreeSingle(expression, e => {
        const d: Map<any> = match.call(helpers, rule.pattern, e)
        if (d) {
            return fire(rule, d);
        }
    });
}
export function tryRules(formula: Expression, rules: Rule[]): Expression {
    let rulesLeft = rules;
    while (true) {
        if (rulesLeft.length === 0) {
            return undefined;
        }
        else {
            const newFormula = tryRule(formula, rulesLeft[0]);
            if (newFormula !== undefined) {
                return newFormula;
            }
            else {
                rulesLeft = rulesLeft.slice(1);
            }
        }
    }
}
export function control(formula: Expression, rules: Rule[][]) {
    while (rules.length) {
        const newFormula = tryRules(formula, rules[0]);
        if (newFormula !== undefined) {
            formula = newFormula;
            continue;
        }
        else {
            rules = rules.slice(1);
        }
    }
    return formula;
            
            
}
