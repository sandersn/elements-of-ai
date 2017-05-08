/// <reference path='../../typings/node.d.ts' />
import { match, Pattern, isAtom } from '../chapter3/match';
import { equal, Map } from '../util';
import * as readline from 'readline';

export interface Variable { type: 'variable', name: string }
export interface Term { type: 'term', name: string, arguments?: Literal[] };
export type Literal = Variable | Term;
export type Substitution = Map<Literal>

export function unify(literal1: Term, literal2: Term): Substitution | 'not-unifiable' {
    let u: Substitution = {};
    if (literal1.name === literal2.name && literal1.arguments && literal2.arguments) {
        try {
            return closure(unifyArguments(literal1.arguments, literal2.arguments, u));
        }
        catch (e) {
            if (e === 'not-unifiable') {
                return 'not-unifiable';
            }
            throw e;
        }
    }
    return 'not-unifiable';
}

function unifyArguments(args1: Literal[], args2: Literal[], u: Substitution) {
    if (args1.length !== args2.length) {
        throw 'not-unifiable';
    }
    for (let i = 0; i < args1.length; i++) {
        u = unify1(args1[i], args2[i], u);
    }
    return u;
}

function unify1(term1: Literal, term2: Literal, u: Substitution): Substitution {
    if (term1 === term2 || areAtomicLiteralsEqual(term1, term2)) {
        return u;
    }
    if (term1.type === 'variable') {
        return addPair(term2, term1, u);
    }
    if (term2.type === 'variable') {
        return addPair(term1, term2, u);
    }
    if (!term1.arguments || !term2.arguments) {
        throw 'not-unifiable';
    }
    // NOTE: The original had a recursive call on name, but this only
    // makes sense if names can be (1) variables or (2) terms.
    // this would be cool but it doesn't match any of the examples, or my
    // understanding of (first-order) predicate calculus.
    if (term1.name !== term2.name) {
        throw 'not-unifiable';
    }
    return unifyArguments(term1.arguments, term2.arguments, u);
}

function areAtomicLiteralsEqual(l1: Literal, l2: Literal) {
    return l1.type === 'term' && !l1.arguments &&
        l2.type === 'term' && !l2.arguments &&
        l1.name === l2.name;
}

function addPair(literal: Literal, variable: Variable, u: Substitution): Substitution {
    if (occursIn(variable, literal)) {
        throw 'not-unifiable';
    }
    if (variable.name in u) {
        const conflict = u[variable.name];
        if (conflict.type === 'variable') {
            // swap the variable -> variable rule to avoid conflict
            u[conflict.name] = { type: 'variable', name: variable.name };
        }
        else {
            throw 'not-unifiable';
        }
    }
    // TODO: Probably doesn't actually need to copy
    let noo: Substitution = {}
    for (const v in u) {
        noo[v] = subst(literal, variable.name, u[v]);
    }
    noo[variable.name] = literal;
    return noo;
}

function occursIn(v: Variable, l: Literal): boolean {
    if (l.type === 'variable' && v.name === l.name) {
        return true;
    }
    else if (l.type === 'variable' || !l.arguments) {
        return false;
    }
    return l.arguments.some(child => occursIn(v, child));
}

function subst(noo: Literal, old: string, l: Literal): Literal {
    if (old === l.name) {
        return noo;
    }
    if (l.type === 'variable' || !l.arguments) {
        return l;
    }
    return {
        type: 'term',
        name: l.name,
        arguments: l.arguments.map(arg => subst(noo, old, arg))
    };
}

export function substitute(l: Literal, u: Substitution): Literal {
    for (const v in u) {
        l = subst(u[v], v, l);
    }
    return l;
}

/**
 * This isn't really transitive closure.
 * At least I'm pretty sure certain chains of variable->variable mappings
 * will not result in all rhs variables being substituted with a term.
 * But it works pretty well for the toy examples here!
 */
function closure(u: Substitution): Substitution {
    for (const v in u) {
        if (u[v].type === 'variable' && u[v].name in u) {
            u[v] = u[u[v].name];
        }
    }
    return u;
}

export function parse(s: string): Literal {
    const tokens = tokenise(s);
    let i = 0;
    const literal = parseLiteral();
    if (i !== tokens.length) {
        throw new Error(`parse error: ${i} is less than ${tokens.length} for ${JSON.stringify(literal)}`);
    }
    return literal;

    function next(): string {
        const s = tokens[i];
        i++;
        return s;
    }
    function parseLiteral(): Literal {
        const token = next();
        // look for name
        if (/\W/.test(token)) {
            throw new Error('expected identifier');
        }
        if (isVariable(token)) {
            return { type: 'variable', name: token };
        }
        else if (tokens[i] === '(') {
            next();
            const args: Literal[] = [];
            let punct: string;
            do {
                args.push(parseLiteral());
                punct = next();
                if (punct !== ',' && punct !== ')') {
                    throw new Error(`expected "," or ")" in argument list, got ${punct} for i=${i}`);
                }
            } while (punct !== ')');
            return { type: 'term', name: token, arguments: args }
        }
        else {
            return { type: 'term', name: token }
        }
    }
    function isVariable(name: string) {
        switch (name) {
            case 'u': case 'v': case 'w':
            case 'x': case 'y': case 'z':
            case 'x1': case 'y1': case 'z1':
            case 'x2': case 'y2': case 'z2':
                return true;
            default:
                return false;
            }
    }
}
function tokenise(s: string): string[] {
    return s.split(/ |(,|\)|\()/).filter(id => id);
}
