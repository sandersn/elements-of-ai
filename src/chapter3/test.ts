/// <reference path="../../typings/jasmine.d.ts"/>
import { isAtom, match2, match3, match4, match5 } from "./match";
import { equal, Map } from "../util";
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
function basicMatchTest(match: (p: any, s: any) => any) {
    it("doesn't match a list against a scalar pattern", () => {
        expect(match(12, [12])).toBeFalsy();
    });
    it("doesn't match a scalar against a real pattern", () => {
        expect(match([12], 12)).toBeFalsy();
    });
    it("matches empty list matches an empty pattern", () => {
        expect(match([], [])).toBeTruthy();
    }); 
    it("non-empty list doesn't match an empty pattern", () => {
        expect(match([], [1])).toBeFalsy();
    }); 
    it("empty list doesn't match a non-empty pattern", () => {
        expect(match([1], [])).toBeFalsy();
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
    it("un-matches recursively", () => {
        expect(match([1, [2, "nope"], 3],
                     [1, [2, "not", "really"], 3])).toBe(false);
    });
}
function basicPatternMatchTest(match: (p: any[], s: any[]) => Map<any>) {
    it("matches a simple wildcard", () => {
        expect(match([1, ["?", "x"], 3], [1, "not really", 3])).toBeTruthy();
    });
    it("has a value for a matching wildcard", () => {
        expect(match([1, ["?", "x"], 3], [1, "not really", 3])["x"]).toBe('not really');
    });
    it("doesn't have a value for a non-matching wildcard", () => {
        expect(match([1, ["?", "x"], 3], [1, "not really", 3])["y"]).toBeFalsy();
    });
    it("is empty for no wildcards", () => {
        expect(match([1, 2, 3], [1, 2, 3])).toEqual({});
    });
    it("is empty for no wildcards", () => {
        expect(match([["?", "x"], ["?", "y"], 3], [1, 2, 3])).toEqual({ x: 1, y: 2 });
    });
    function doesntMatch(p: any, s: any) {
        it("doesn't match " + p , () => {
            expect(match(p, s)).toBeFalsy();
        });
    }
    doesntMatch([1,3], [1,2]);
    doesntMatch([1, ["?"], 3], [1, [2], 3]);
    doesntMatch([1, ["?", 'x', 'y'], 3], [1, [2], 3]);
    doesntMatch([1, ["!", 'x'], 3], [1, [2], 3]);
    doesntMatch([["?", 'x'], 3], [1, 2]);
}
function functionPatternMatchTest(match: (p: any[], s: any[]) => Map<any>) {
    const functions: Map<(x: number) => boolean> = {
        isEqual1: x => x == 1
    };
    it("matches simple functions", () => {
        expect(match.call(functions,
                          [["isEqual1", 'x'], ["?", 'y'], 3],
                          [1, 2, 3]))
            .toEqual({ x: 1, y: 2 });
    });
    it("doesn't match failing functions", () => {
        expect(match.call(functions,
                          [["isEqual1", 'x'], ["?", 'y'], 3],
                          [4, 2, 3]))
            .toBeFalsy();
    });
}
describe("equal", () => {
    assert(false, equal([1], null), "[1] /= []");
    assert(false, equal(null, [1]), "[] /= [1]");
    assert(false, equal([1], [1,2]), "[1] /= [1,2]");
    assert(false, equal([1,2], [1]), "[1,2] /= [1]");
    assert(false, equal([1,4], [1,3]), "[1,4] /= [1,3]");
    assert(false, equal([4,1], [3,1]), "[4,1] /= [3,1]");
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
describe("match4", () => {
    basicMatchTest(match4);
    basicPatternMatchTest(match4);
    it("allows malformed patterns as literals", () => {
        expect(match4([1, [2], 3], [1, [2], 3])).toEqual({});
    });
});
describe("match5", () => {
    basicMatchTest(match5);
    basicPatternMatchTest(match5);
    functionPatternMatchTest(match5);
    it("allows malformed patterns as literals", () => {
        expect(match4([1, [2], 3], [1, [2], 3])).toEqual({});
    });
});
