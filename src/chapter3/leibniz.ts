import { Pattern, isAtom, match } from "./match";
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
    diffPowerE3Rule: binaryDiffHelper(["exp", ["?", 'e1'], ['isNumber', 'e2']])
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
                  ["*", d['e2'], ['d', d['e1']], d['v1']],
                  ["*", d['e1'], ['d', d['e2']], d['v1']]],
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
                  
export const rules: Rule[][] = [
    [diffSumRule, diffXRule, diffConstRule, diffProductRule, diffPowerRule],
    [/*laddZeroRule, raddZeroRule, addConstRule, lprodZeroRule, rprodZeroRule, lprodIdRule, rProdIdRule, prodConstRule, expZeroRule, expIdRule, decRule*/]]

/// control scheme ///
function tryRuleOnList(rule: Rule, expressions: Expression[]): Expression[] {
    if (expressions.length === 0) return undefined;
    const temp = tryRule(rule, expressions[0]);
    if (temp) {
        // TODO: Copy and replace instead.
        return [temp].concat(expressions.slice(1));
    }
    else {
        const temp2 = tryRuleOnList(rule, expressions.slice(1));
        if (temp2) {
            return [expressions[0]].concat(temp);
        }
    }
}
function fire(rule: Rule, d: State) {
    return rule.action(d);
}
export function tryRule(rule: Rule, expression: Expression): Expression {
    if (isAtom(expression)) return undefined;

    console.log("It's not an atom");
    const d: Map<any> = match.call(helpers, rule.pattern, expression);
    console.log(`Did ${rule.pattern} match ${expression}?`);
    if (d) {
        console.log('yes');
        return fire(rule, d);
    }
    else {
        console.log('no');
        return tryRuleOnList(rule, expression);
    }
}
export function tryRules(rules: Rule[], formula: Expression): Expression {
    let rulesLeft = rules;
    while (true) {
        if (rulesLeft.length === 0) {
            return undefined;
        }
        else {
            const newFormula = tryRule(rulesLeft[0], formula);
            if (newFormula) {
                return newFormula;
            }
            else {
                rulesLeft = rulesLeft.slice(1);
            }
        }
    }
}
function control(formula: Expression, rules: Rule[][]) {
    while (true) {
        // TODO: Probably wrong!
        const newFormula = tryRules(rules[0], formula);
        if (newFormula) {
            formula = newFormula;
            continue;
        }
        else if (rules.length === 0) {
            break;
        }
        else {
            rules = rules.slice(1);
        }
    }
            
            
}
