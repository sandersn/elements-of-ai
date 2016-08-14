/// <reference path="../../typings/jasmine.d.ts"/>
import { equal, Map } from "../util";
import { rotateList, orient, matchNorth, Pattern, Orientation } from "./painted-squares";
describe("rotateList", () => {
    const l = [1, 2, 3];
    it("no-ops 0", () => {
        expect(rotateList(l, 0)).toEqual([1,2,3]);
    });
    it("rotates 1", () => {
        expect(rotateList(l, 1)).toEqual([3, 1, 2]);
    });
    it("rotates 2", () => {
        expect(rotateList(l, 2)).toEqual([2, 3, 1]);
    });
    it("rotates all the way around", () => {
        expect(rotateList(l, 3)).toEqual([1, 2, 3]);
    });
    it("keeps on rotating past max length", () => {
        expect(rotateList(l, 4)).toEqual([3, 1, 2]);
        expect(rotateList(l, 5)).toEqual([2, 3, 1]);
    });
});
describe("orient", () => {
    it("rotates piece 1 South (no-op)", () => {
        expect(orient(['p1', Orientation.S])).toEqual(
            [Pattern.ST, Pattern.HA, Pattern.GR, Pattern.ST]);
    });
    it("rotates piece 1 east", () => {
        expect(orient(['p1', Orientation.E])).toEqual(
            [Pattern.ST, Pattern.ST, Pattern.HA, Pattern.GR]);
    });
});
describe("matchNorth", () => {
    it("matches unrotated piece", () => {
        expect(matchNorth([Pattern.None, Pattern.None, Pattern.ST, Pattern.None],
                          [['--', Orientation.N],
                           ['p1', Orientation.S]])).toBeTruthy();
    });
    it("matches rotated piece", () => {
        expect(matchNorth([Pattern.None, Pattern.None, Pattern.GR, Pattern.None],
                          [['--', Orientation.N],
                           ['p1', Orientation.N]])).toBeTruthy();
    });
    it("doesn't match incorrect pattern", () => {
        expect(matchNorth([Pattern.None, Pattern.None, Pattern.ST, Pattern.None],
                          [['--', Orientation.N],
                           ['p1', Orientation.N]])).toBeFalsy();
    });
});
