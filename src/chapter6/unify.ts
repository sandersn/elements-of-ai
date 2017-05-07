/// <reference path='../../typings/node.d.ts' />
import { match, Pattern, isAtom } from '../chapter3/match';
import { equal } from '../util';
import * as readline from 'readline';

type Literal =
    | { type: 'variable', name: string }
    | { type: 'term', name: string, arguments?: Literal[] }

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
