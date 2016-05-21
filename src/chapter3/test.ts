/// <reference path="../../typings/jasmine.d.ts"/>
import { isAtom, match2 } from "./match";
import { l } from "../util";
import * as Cons from "jscons";
describe("isAtom", () => {
    it("says number is an atom", () => {
        expect(isAtom(12)).toBe(true);
    });
    it("says empty cons is an atom", () => {
        expect(isAtom(Cons.from([]))).toBe(true);
    });
    it("says empty array is an atom", () => {
        expect(isAtom([])).toBe(true);
    });
    it("says empty object is an atom", () => {
        expect(isAtom({})).toBe(true);
    });
    it("says null is an atom", () => {
        expect(isAtom(null)).toBe(true);
    });
    it("says cons on to Cons.from is not an atom", () => {
        expect(isAtom(new Cons(1, Cons.from([2, 3])))).toBe(false);
    });
    it("says true is an atom", () => {
        expect(isAtom(true)).toBe(true);
    });
});
function basicMatchTest(match: (p: any, s: any) => boolean) {
    it("doesn't match a list against a scalar pattern", () => {
        expect(match(12, l(12))).toBe(false);
    });
    it("doesn't match a scalar against a real pattern", () => {
        expect(match(l(12), 12)).toBe(false);
    });
    it("empty list matches an empty pattern", () => {
        expect(match(null, null)).toBe(true);
    }); 
    it("non-empty list doesn't match an empty pattern", () => {
        expect(match(null, l(1))).toBe(false);
    }); 
    it("empty list doesn't match a non-empty pattern", () => {
        expect(match(l(1), null)).toBe(false);
    }); 
}
function recursiveMatchTest(match: (p: any, s: any) => boolean) {
    it("matches recursively", () => {
        expect(match(l(1, l(2), 3), l(1, l(2), 3))).toBe(true);
    });
    it("un-matches recursively", () => {
        expect(match(l(1, l(2, "nope"), 3),
                     l(1, l(2, "not", "really"), 3))).toBe(false);
    });
}
describe("match2", () => {
    it("matches bare atoms", () => {
        expect(match2(12, 12)).toBe(true);
    });
    basicMatchTest(match2);
    recursiveMatchTest(match2);
});
