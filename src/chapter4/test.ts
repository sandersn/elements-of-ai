/// <reference path="../../typings/jasmine.d.ts"/>
import { equal, Map } from "../util";
import { addToList, isaTest, isArticle, makeConj } from "./linneus";
describe("addToList", () => {
    it("adds to an empty list", () => {
        const d: Map<number[]> = {};
        addToList('a', 1, d);
        expect(d).toEqual({a: [1]});
    });
    it("adds to an existing list", () => {
        const d: Map<number[]> = {a: [1]};
        addToList('a', 2, d);
        expect(d).toEqual({a: [1, 2]});
    });
});
describe("isaTest", () => {
    it("says Dog ISA Dog", () => {
        expect(isaTest({}, 'dog', 'dog', 100)).toBeTruthy();
    });
    it("says Dog not ISA Cat", () => {
        expect(isaTest({}, 'dog', 'cat', 100)).toBeFalsy();
    });
    it("says Dog ISA Mammal", () => {
        expect(isaTest({dog: ['mammal']}, 'dog', 'mammal', 100)).toBeTruthy();
    });
    it("says Dog not ISA Bug", () => {
        expect(isaTest({dog: ['mammal']}, 'dog', 'bug', 100)).toBeFalsy();
    });
    it("says Dog ISA Animal with limit=2", () => {
        expect(isaTest({dog: ['mammal'], mammal: ['animal']}, 'dog', 'animal', 2)).toBeTruthy();
    });
    it("says Dog not ISA Animal with limit=1", () => {
        expect(isaTest({dog: ['mammal'], mammal: ['animal']}, 'dog', 'animal', 1)).toBeFalsy();
    });

});
describe("isArticle", () => {
    it("says 'a' is an article", () => {
        expect(isArticle('a')).toBeTruthy();
    });
    it("says 'wat' is not an article", () => {
        expect(isArticle('wat')).toBeFalsy();
    });
});
describe("makeConj", () => {
    const basic: Map<string> = { 'dog': 'a', 'animal': 'an' };
    it("conjoins an empty list to an empty list", () => {
        expect(makeConj([], basic)).toEqual("");
    });
    it("adds articles, even to singletons", () => {
        expect(makeConj(['dog'], basic)).toEqual("a dog");
    });
    it("conjoins multiple items", () => {
        expect(makeConj(['dog', 'animal'], basic)).toEqual("a dog and an animal");
    });
});
