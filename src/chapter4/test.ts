/// <reference path="../../typings/jasmine.d.ts"/>
import { equal, Map } from "../util";
import { addToList, isaTest, isArticle, makeConj, explainLinks, interpret, chainInterpret, caseMatch } from "./linneus";
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
describe("explainLinks", () => {
    it("explains identical things", () => {
        expect(explainLinks({}, {}, 'dog', 'dog')).toBe("They are identical");
    });
    it("explains given things", () => {
        expect(explainLinks({'dog': ['animal']}, {'dog': 'a'}, 'dog', 'animal')).toBe("You told me");
    });
    it("explains chains", () => {
        expect(explainLinks({'dog': ['mammal'], 'mammal': ['animal']},
                            {'dog': 'a', 'mammal': 'a', 'animal': 'an' },
                            'dog',
                            'animal')).toBe("a dog is a mammal and a mammal is an animal");
    });
});
describe("case-match", () => {
    it('matches basic facts', () => {
        expect(caseMatch("a dog is an animal".split(' '), [[[['a', ["?", 'x'], 'is', 'an', ["?", 'y']]],
        d => 'add-fact']])).toBe('add-fact');
    });
    it('matches articles', () => {
        expect(caseMatch("a dog is an animal".split(' '), [[[[['isArticle', 'article1'], ["?", 'x'], 'is', ['isArticle', 'article1'], ["?", 'y']]],
        d => 'add-fact']])).toBe('add-fact');
    });
});
describe("interpret", () => {
    it('actually adds facts', () => {
        let isa: Map<string[]> = {};
        let includes: Map<string[]> = {};
        let articles: Map<string> = {};
        expect(interpret("a dog is an animal".split(' '), isa, includes, articles)).toEqual('add-fact');
        expect(isa).toEqual({ dog: ["animal"] });
        expect(includes).toEqual({ animal: ["dog"] });
        expect(articles).toEqual({ dog: 'a', animal: 'an' });
    });
    it('adds facts', () => {
        expect(chainInterpret(["a dog is an animal"])).toEqual(
            ['add-fact',
             {dog: ["animal"]},
             {animal: ["dog"]},
             {dog: 'a', animal: 'an'}]);
    });
    it('adds multiple facts', () => {
        expect(chainInterpret(["a mammal is an animal", "a dog is a mammal"])).toEqual(
            ['add-fact',
            { dog: ["mammal"], mammal: ["animal"] },
            { mammal: ["dog"], animal: ["mammal"] },
            { dog: 'a', animal: 'an', mammal: 'a' }]);
    });
    it('answers includes questions', () => {
        expect(chainInterpret(["a dog is an animal", "what is an animal"])).toEqual(
            ['includes',
            { dog: ["animal"] },
            { animal: ["dog"] },
            { dog: 'a', animal: 'an' }]);
    });
    it('answers incorrect includes questions', () => {
        expect(chainInterpret(["a dog is an animal", "what is a a wombat"])).toEqual(
            ['dunno',
                { dog: ["animal"] },
                { animal: ["dog"] },
                { dog: 'a', animal: 'an' }]);
    });
    it('answers multiple isa questions', () => {
        expect(chainInterpret(["a dog is an animal", "a dog is a friend", "what is a dog"])).toEqual(
            ['isa',
                { dog: ["animal", "friend"] },
                { friend: ["dog"], animal: ["dog"] },
                { dog: 'a', animal: 'an', friend: 'a' }]);
    });
    it('answers because questions', () => {
        expect(chainInterpret(["a mammal is an animal", "a dog is a mammal", "why is a dog an animal"])).toEqual(
            ['because',
                { dog: ["mammal"], mammal: ["animal"] },
                { mammal: ["dog"], animal: ["mammal"] },
                { dog: 'a', animal: 'an', mammal: 'a' }]);
    });
    it('answers incorrect because questions', () => {
        expect(chainInterpret(["a mammal is an animal", "a dog is a mammal", "why is a wombat an animal"])).toEqual(
            ['its-not',
                { dog: ["mammal"], mammal: ["animal"] },
                { mammal: ["dog"], animal: ["mammal"] },
                { dog: 'a', animal: 'an', mammal: 'a' }]);
    });
});

