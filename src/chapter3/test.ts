/// <reference path="../../typings/jasmine.d.ts"/>
import { isAtom } from "./match";
import Cons = require("jscons");
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
    it("says null is not an atom", () => {
        expect(isAtom(null)).toBe(false);
    });
    it("says cons on to Cons.from is not an atom", () => {
        expect(isAtom(new Cons(1, Cons.from([2, 3])))).toBe(false);
    });
    it("says true is an atom", () => {
        expect(isAtom(true)).toBe(true);
    });
});
