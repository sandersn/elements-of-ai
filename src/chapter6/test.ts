/// <reference path="../../typings/jasmine.d.ts"/>
import { equal, Map } from "../util";
import { parse, format, valid, prove, normalise, wff } from "./prover"
describe("prove", () => {
    it("validates intersection", () => {
        // expect(prove([[]])).toEqual("VALID");
    });
    it("passes the example", () => {
        // expect(prove(parse("(a & (not b)) -> a"))).toEqual("VALID");
    });
});
function runValidate(ls: string[], rs: string[], equals: boolean) {
    expect(valid(ls.map(parse), rs.map(parse))).toEqual(equals);
}
function run(x: Map<[string[], string[], boolean]>) {
    for (const k in x) {
        it(k, () => {
            const [ ls, rs, result ] = x[k];
            runValidate(ls, rs, result);
        });
    }
}
describe("valid", () => {
    run({
        "validates intersection": [['x'], ['x', 'x & y'], true],
        "negative validates intersection": [['x'], ['x & y'], false],
        "validates negation (l)": [['x', 'not x'], ['x & y'], true],
        "validates negation (l2)": [['not x', 'x', ], ['x & y'], true],
        "validates negation (r)": [['x & y'], ['x', 'not x'], true],
        "validates negation (r2)": [['x & y'], ['not x', 'x'], true],
        // on the right, should transform x | y to x, y and then match
        "validates or (r, lhs)": [['x'], ['x | y'], true],
        "validates or (r, rhs)": [['y'], ['x | y'], true],
        // on the left, should transform x & y to x, y and then match
        "validates and (l, lhs)": [['x & y'], ['x'], true],
        "validates and (l, rhs)": [['x & y'], ['y'], true],
    });
});
describe("normalise", () => {
    it("is identity for atoms", () => {
        expect(normalise('x')).toEqual('x');
    });
    it("throws an error for ill-formed expressions", () => {
        expect(() => normalise(['singleton'])).toThrowError("syntax error");
    });
    it("rewrites implication", () => {
        expect(normalise(parse("x -> y"))).toEqual(parse("(not x) | y"));
    });
    it("rewrites nested implications", () => {
        expect(normalise(parse("(x -> y) -> z -> alpha"))).toEqual(
            parse("(not ((not x) or y)) or ((not z) or alpha)"));
    });
});
describe("wff", () => {
    it("is true for atoms", () => {
        expect(wff('x')).toEqual(true);
    });
    it("is false for 1-element lists", () => {
        expect(wff(['x'])).toEqual(false);
    });
    it("is true for 2-element not-lists", () => {
        expect(wff(parse("not x"))).toEqual(true);
    });
    it("is false for other 2-element lists", () => {
        expect(wff(['x', 'y'])).toEqual(false);
    });
    it("is true for 3-element operator expressions", () => {
        expect(wff(parse("x => y"))).toEqual(true);
    });
    it("is false for other 3-element expressions", () => {
        expect(wff(['x', 'whatever', 'y'])).toEqual(false);
    });
    it("is true for well-formed nested expressions", () => {
        expect(wff(parse("(not x) & (y -> x)"))).toEqual(true);
    });
});
describe("parse", () => {
    it("parses x => y", () => {
        expect(parse("x -> y")).toEqual(['x', 'implies', 'y']);
    });
    it("parses (x) => y", () => {
        expect(parse("(x) => y")).toEqual(['x', 'implies', 'y']);
    });
    it("parses not x & y", () => {
        expect(parse("not x & y")).toEqual(['not', ['x', 'and', 'y']]);
    });
    it("parses (not x) & y", () => {
        expect(parse("(not x) & y")).toEqual([['not', 'x'], 'and', 'y']);
    });
    it("parses (not x) and (y -> x)", () => {
        expect(parse("(not x) and y -> x")).toEqual([["not", 'x'], 'and', ['y', 'implies', 'x']]);
    });
    it("parses (not x) and (y -> x)", () => {
        expect(parse("(not x) and y -> x")).toEqual([["not", 'x'], 'and', ['y', 'implies', 'x']]);
    });
    it("parses x -> y -> z -> alpha", () => {
        expect(parse("x -> y -> z -> alpha")).toEqual(['x', 'implies', ['y', 'implies', ['z', 'implies', 'alpha']]]);
    });
    it("parses (x -> y) -> z -> alpha", () => {
        expect(parse("(x -> y) -> z -> alpha")).toEqual([['x', 'implies', 'y'], 'implies', ['z', 'implies', 'alpha']]);
    });

});
describe("format", () => {
    it("formats x=> y", () => {
        expect(format(parse("x -> y"))).toEqual("(x implies y)");
    });
    it("formats (not x) => y", () => {
        expect(format(parse("(not x) -> y"))).toEqual("((not x) implies y)");
    });
});
