/// <reference path="../../typings/jasmine.d.ts"/>
import { equal, Map } from "../util";
import { parse as parseProver, format, valid, prove, normalise, wff } from "./prover"
import { parse as parseUnify, unify, substitute, Literal, Term, Substitution } from "./unify"
describe("prove", () => {
    it("passes the example", () => {
        expect(prove(parseProver("(a & (not b)) -> a"))).toEqual("VALID");
    });
});
function runValidate(ls: string[], rs: string[], equals: boolean) {
    expect(valid(ls.map(parseProver), rs.map(parseProver))).toEqual(equals);
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
        "invalidates intersection": [['x'], ['x & y'], false],
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
        // on the left, should transform x | y -> x to x -> x && y -> x
        "validates or (l)": [["x | y"], ["x", "y"], true],
        "invalidates or (l)": [["x | y"], ["y"], false],
        // on the right, should transform x -> x & y to x -> x && x -> y
        "validates and (r)": [["x", "y"], ["x & y"], true],
        "invalidates and (r)": [["x"], ["x & y"], false],
        "validates the book's example (1)": [['(not p) | q', '(not q) | r', 'not r'],
                                             ["not p"],
                                             true],
        "validates the book's example (2)": [['a | b', 'not (b & c)'],
                                             ["(not c) | a"],
                                             true],
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
        expect(normalise(parseProver("x -> y"))).toEqual(parseProver("(not x) | y"));
    });
    it("rewrites nested implications", () => {
        expect(normalise(parseProver("(x -> y) -> z -> alpha"))).toEqual(
            parseProver("(not ((not x) or y)) or ((not z) or alpha)"));
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
        expect(wff(parseProver("not x"))).toEqual(true);
    });
    it("is false for other 2-element lists", () => {
        expect(wff(['x', 'y'])).toEqual(false);
    });
    it("is true for 3-element operator expressions", () => {
        expect(wff(parseProver("x => y"))).toEqual(true);
    });
    it("is false for other 3-element expressions", () => {
        expect(wff(['x', 'whatever', 'y'])).toEqual(false);
    });
    it("is true for well-formed nested expressions", () => {
        expect(wff(parseProver("(not x) & (y -> x)"))).toEqual(true);
    });
});
describe("parseProver", () => {
    it("parses x", () => {
        expect(parseProver("x")).toEqual('x');
    });
    it("parses x => y", () => {
        expect(parseProver("x -> y")).toEqual(['x', 'implies', 'y']);
    });
    it("parses (x) => y", () => {
        expect(parseProver("(x) => y")).toEqual(['x', 'implies', 'y']);
    });
    it("parses not x & y", () => {
        expect(parseProver("not x & y")).toEqual(['not', ['x', 'and', 'y']]);
    });
    it("parses (not x) & y", () => {
        expect(parseProver("(not x) & y")).toEqual([['not', 'x'], 'and', 'y']);
    });
    it("parses (not x) and (y -> x)", () => {
        expect(parseProver("(not x) and y -> x")).toEqual([["not", 'x'], 'and', ['y', 'implies', 'x']]);
    });
    it("parses (not x) and (y -> x)", () => {
        expect(parseProver("(not x) and y -> x")).toEqual([["not", 'x'], 'and', ['y', 'implies', 'x']]);
    });
    it("parses x -> y -> z -> alpha", () => {
        expect(parseProver("x -> y -> z -> alpha")).toEqual(['x', 'implies', ['y', 'implies', ['z', 'implies', 'alpha']]]);
    });
    it("parses (x -> y) -> z -> alpha", () => {
        expect(parseProver("(x -> y) -> z -> alpha")).toEqual([['x', 'implies', 'y'], 'implies', ['z', 'implies', 'alpha']]);
    });

});
describe("format", () => {
    it("formats x=> y", () => {
        expect(format(parseProver("x -> y"))).toEqual("(x implies y)");
    });
    it("formats (not x) => y", () => {
        expect(format(parseProver("(not x) -> y"))).toEqual("((not x) implies y)");
    });
});

describe("parseUnify", () => {
    it("parses basic non-nested literal", () => {
        expect(parseUnify("P(b, x)")).toEqual({
            type: "term",
            name: "P",
            arguments: [
                { type: "term", name: "b" },
                { type: "variable", name: "x" },
            ]
        });
    });
    it("parses nested literal", () => {
        expect(parseUnify("P(f(h(b)), g(x, y))")).toEqual({
            type: "term",
            name: "P",
            arguments: [{
                type: "term",
                name: "f",
                arguments: [{
                    type: "term",
                    name: "h",
                    arguments: [ { type: "term", name: "b" } ]
                }]
            }, {
                type: "term",
                name: "g",
                arguments: [
                    { type: "variable", name: "x" },
                    { type: "variable", name: "y" }
                ]
            }]
        });
    });
});

describe("unify", () => {
    it("trivially unifies identical literals", () => {
        const l1 = parseUnify("P(b)") as Term;
        const l2 = parseUnify("P(b)") as Term;
        expect(unify(l1, l2)).toEqual({});
    });
    it("unifies a simple variable", () => {
        const l1 = parseUnify("P(b)") as Term;
        const l2 = parseUnify("P(x)") as Term;
        expect(unify(l1, l2)).toEqual({
            x: { type: 'term', name: 'b' }
        });
    });
    it("passes the first test", () => {
        const l1 = parseUnify("P(x, f(a))") as Term;
        const l2 = parseUnify("P(b, y)") as Term;
        expect(unify(l1, l2)).toEqual({
            y: { type: 'term', name: 'f', arguments: [{type: 'term', name: 'a'}] },
            x: { type: 'term', name: 'b' },
        });
    });
    it("passes the second test", () => {
        const l1 = parseUnify("P(f(x), g(a, x))") as Term;
        const l2 = parseUnify("P(f(h(b)), g(x, y))") as Term;
        expect(unify(l1, l2)).toEqual('not-unifiable');
    });
    it("passes the fourth test", () => {
        const l1 = parseUnify("P(x, f(y), x)") as Term;
        const l2 = parseUnify("P(z, f(z), a)") as Term;
        expect(unify(l1, l2)).toEqual({
            x: { type: 'term', name: 'a' },
            y: { type: 'term', name: 'a' },
            z: { type: 'term', name: 'a' }
        });
    });
    it("produces a correct unification", () => {
        const l1 = parseUnify("P(x, f(y), x)") as Term;
        const l2 = parseUnify("P(z, f(z), a)") as Term;
        expect(substitute(l1, unify(l1, l2) as Substitution)).toEqual({
            type: 'term',
            name: 'P',
            arguments: [
                { type: 'term', name: 'a' },
                { type: 'term', name: 'f', arguments: [ { type: 'term', name: 'a' } ] },
                { type: 'term', name: 'a' },
            ]
        });
        expect(substitute(l2, unify(l1, l2) as Substitution)).toEqual({
            type: 'term',
            name: 'P',
            arguments: [
                { type: 'term', name: 'a' },
                { type: 'term', name: 'f', arguments: [ { type: 'term', name: 'a' } ] },
                { type: 'term', name: 'a' },
            ]
        });
    });
    it("passes the example", () => {
        const l1 = parseUnify("A(x, f(y))") as Term;
        const l2 = parseUnify("A(a, f(g(z)))") as Term;
        expect(unify(l1, l2)).toEqual({
            y: { type: 'term', name: 'g', arguments: [ { type: 'variable', name: 'z' } ] },
            x: { type: 'term', name: 'a' },
        });
    });
    it("fails the occurs check", () => {
        const l1 = parseUnify("P(x)") as Term;
        const l2 = parseUnify("P(f(x))") as Term;
        expect(unify(l1, l2)).toEqual('not-unifiable');
    });
});
