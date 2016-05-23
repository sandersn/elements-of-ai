/// <reference path="../../typings/jasmine.d.ts"/>
import { isAtom, match2, match3 } from "./match";
import { equal } from "../util";
function assert<T>(expected: T, actual: T, message: string) {
    it(message, () => { expect(actual).toBe(expected); });
}
describe("isAtom", () => {
    it("says number is an atom", () => {
        expect(isAtom(12)).toBe(true);
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
    it("says concanenated array is not an atom", () => {
        expect(isAtom([1].concat([2, 3]))).toBe(false);
    });
    it("says true is an atom", () => {
        expect(isAtom(true)).toBe(true);
    });
});
function basicMatchTest(match: (p: any, s: any) => boolean) {
    it("doesn't match a list against a scalar pattern", () => {
        expect(match(12, [12])).toBe(false);
    });
    it("doesn't match a scalar against a real pattern", () => {
        expect(match([12], 12)).toBe(false);
    });
    it("matches empty list matches an empty pattern", () => {
        expect(match([], [])).toBe(true);
    }); 
    it("non-empty list doesn't match an empty pattern", () => {
        expect(match([], [1])).toBe(false);
    }); 
    it("empty list doesn't match a non-empty pattern", () => {
        expect(match([1], [])).toBe(false);
    }); 
}
function recursiveMatchTest(match: (p: any, s: any) => boolean) {
    it("matches recursively", () => {
        expect(match([1, [2], 3], [1, [2], 3])).toBe(true);
    });
    it("un-matches recursively", () => {
        expect(match([1, [2, "nope"], 3],
                     [1, [2, "not", "really"], 3])).toBe(false);
    });
}
describe("equal", () => {
    assert(false, equal([1], null), "[1] /= []");
    assert(false, equal(null, [1]), "[] /= [1]");
    assert(false, equal([1], [1,2]), "[1] /= [1,2]");
    assert(false, equal([1,2], [1]), "[1,2] /= [1]");
    assert(true, equal([1, [2], 3], [1, [2], 3]), "[1,2,3] == [1,2,3]");
});
describe("match2", () => {
    it("matches bare atoms", () => {
        expect(match2(12, 12)).toBe(true);
    });
    basicMatchTest(match2);
    recursiveMatchTest(match2);
});
describe("match3", () => {
    basicMatchTest(match3);
    recursiveMatchTest(match3);
});
