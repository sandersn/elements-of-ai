/// <reference path="../../typings/jasmine.d.ts"/>
import { equal, Map } from "../util";
import { rotateList, orient, matchNorth, matchWest, sidesOk, solveSquares, solve, Pattern, Orientation } from "./painted-squares";
import { france, franceDistance, depthFirstSearch, breadthFirstSearch, bestFirstSearch, uniformCost, aStar, longitudeDifference, longitudeEstimate, Intercity, Distance } from "./search";
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
describe("matchWest", () => {
    const allstripe = [Pattern.ST, Pattern.ST, Pattern.ST, Pattern.ST];
    const allhash = [Pattern.HA, Pattern.HA, Pattern.HA, Pattern.HA];
    it("matches piece rotated once", () => {
        expect(matchWest(allstripe,
                          [['p1', Orientation.E]])).toBeTruthy();
    });
    it("matches piece unrotated", () => {
        expect(matchWest(allhash,
                          [['p1', Orientation.S]])).toBeTruthy();
    });
    it("doesn't match incorrect pattern (last piece)", () => {
        expect(matchWest(allstripe,
                         [['p1', Orientation.S],
                          ['--', Orientation.N],
                          ['--', Orientation.N]])).toBeFalsy();
    });
    it("doesn't match incorrect pattern (second piece)", () => {
        expect(matchWest(allhash,
                         [['p1', Orientation.E]])).toBeFalsy();
    });
    it("matches a correct piece", () => {
        expect(matchWest([Pattern.None, Pattern.None, Pattern.None, Pattern.ST],
                         [['p1', Orientation.E]])).toBeTruthy();
    });
    it("matches a correct piece", () => {
        expect(matchWest([Pattern.None, Pattern.None, Pattern.None, Pattern.HA],
                         [['p1', Orientation.S]])).toBeTruthy();
    });
});
describe("sidesOk", () => {
    it("places the first unrotated piece", () => {
        expect(sidesOk(['p1', Orientation.S], [])).toBeTruthy();
    });
    it("places the first rotated piece", () => {
        expect(sidesOk(['p2', Orientation.E], [])).toBeTruthy();
    });
    it("places the third unrotated piece", () => {
        expect(sidesOk(['p1', Orientation.S], [
            ['--', Orientation.S],
            ['p3', Orientation.E]])).toBeTruthy();
    });
    it("places the third unrotated piece (2)", () => {
        expect(sidesOk(['p1', Orientation.S], [
            ['--', Orientation.S],
            ['p4', Orientation.S]])).toBeTruthy();
    });
    it("places the second unrotated piece", () => {
        expect(sidesOk(['p1', Orientation.S], [
            ['p3', Orientation.E]])).toBeTruthy();
    });
    it("places the second unrotated piece (2)", () => {
        expect(sidesOk(['p1', Orientation.S], [
            ['p2', Orientation.S]])).toBeTruthy();
    });
    it("places the fourth unrotated piece", () => {
        expect(sidesOk(['p1', Orientation.S], [
            ['p2', Orientation.S],
            ['p4', Orientation.S],
            ['--', Orientation.S]])).toBeTruthy();
    });
});
describe("solves", () => {
    it("works", () => {
        expect(solve()).toEqual(
            [   [ [ 'p4', 3 ], [ 'p3', 2 ], [ 'p2', 1 ], [ 'p1', 0 ] ]
              , [ [ 'p4', 0 ], [ 'p2', 1 ], [ 'p3', 3 ], [ 'p1', 1 ] ]
              , [ [ 'p4', 0 ], [ 'p3', 3 ], [ 'p2', 2 ], [ 'p1', 2 ] ]
              , [ [ 'p3', 1 ], [ 'p4', 2 ], [ 'p2', 2 ], [ 'p1', 2 ] ]
              , [ [ 'p2', 0 ], [ 'p4', 2 ], [ 'p3', 3 ], [ 'p1', 2 ] ]
              , [ [ 'p3', 3 ], [ 'p2', 0 ], [ 'p4', 3 ], [ 'p1', 3 ] ]
              , [ [ 'p3', 2 ], [ 'p4', 3 ], [ 'p1', 0 ], [ 'p2', 0 ] ]
              , [ [ 'p3', 3 ], [ 'p1', 1 ], [ 'p4', 0 ], [ 'p2', 2 ] ]
              , [ [ 'p1', 0 ], [ 'p3', 1 ], [ 'p4', 0 ], [ 'p2', 2 ] ]
              , [ [ 'p4', 2 ], [ 'p3', 2 ], [ 'p1', 2 ], [ 'p2', 3 ] ]
              , [ [ 'p4', 3 ], [ 'p1', 3 ], [ 'p3', 2 ], [ 'p2', 3 ] ]
              , [ [ 'p3', 0 ], [ 'p1', 3 ], [ 'p4', 1 ], [ 'p2', 3 ] ]
              , [ [ 'p4', 3 ], [ 'p1', 3 ], [ 'p2', 1 ], [ 'p3', 0 ] ]
              , [ [ 'p2', 2 ], [ 'p1', 2 ], [ 'p4', 1 ], [ 'p3', 0 ] ]
              , [ [ 'p2', 0 ], [ 'p4', 2 ], [ 'p1', 3 ], [ 'p3', 1 ] ]
              , [ [ 'p1', 1 ], [ 'p4', 1 ], [ 'p2', 2 ], [ 'p3', 1 ] ]
              , [ [ 'p2', 1 ], [ 'p4', 3 ], [ 'p1', 1 ], [ 'p3', 2 ] ]
              , [ [ 'p1', 0 ], [ 'p2', 0 ], [ 'p4', 0 ], [ 'p3', 3 ] ]
              , [ [ 'p2', 1 ], [ 'p1', 0 ], [ 'p3', 0 ], [ 'p4', 0 ] ]
              , [ [ 'p3', 2 ], [ 'p2', 3 ], [ 'p1', 1 ], [ 'p4', 1 ] ]
              , [ [ 'p2', 1 ], [ 'p3', 0 ], [ 'p1', 1 ], [ 'p4', 1 ] ]
              , [ [ 'p1', 2 ], [ 'p2', 3 ], [ 'p3', 0 ], [ 'p4', 1 ] ]
              , [ [ 'p1', 3 ], [ 'p3', 1 ], [ 'p2', 3 ], [ 'p4', 2 ] ]
              , [ [ 'p1', 0 ], [ 'p2', 0 ], [ 'p3', 1 ], [ 'p4', 2 ] ] ]);
    });
});

describe("depthFirstSearch", () => {
    it("works on France", () => {
        expect(depthFirstSearch(france, 'rennes', 'avignon')).toEqual(
            ['rennes', 'caen', 'calais', 'nancy', 'strasbourg', 'dijon', 'lyon', 'grenoble', 'avignon']);
    });
});
describe("breadthFirstSearch", () => {
    it("works on France", () => {
        expect(breadthFirstSearch(france, 'rennes', 'avignon')).toEqual(
            [['rennes', 'paris', 'dijon', 'lyon', 'avignon'], 15]);
    });
});
describe("bestFirstSearch", () => {
    it("works on France", () => {
        expect(bestFirstSearch(france,
                               n => longitudeDifference(n, 'avignon'),
                               'rennes',
                               'avignon')).toEqual(
                                   [['rennes', 'paris', 'dijon', 'lyon', 'avignon'], 12]);
    });
});
describe("uniformCost", () => {
    it("works on France", () => {
        expect(uniformCost(franceDistance as Map<[string, Intercity][]>,
                           'rennes',
                           'avignon')).toEqual(
                               [['rennes', 'nantes', 'limoges', 'lyon', 'avignon'], 19]);
    });
});
describe("A*", () => {
    // TODO: test with distance = id 0 and longitudeEstimate
    it("works on France with no estimate", () => {
        expect(aStar(franceDistance as Map<[string, Intercity][]>,
                          () => 0 as Distance,
                          'rennes',
                          'avignon')).toEqual(
                              [['rennes', 'nantes', 'limoges', 'lyon', 'avignon'], 16]);
    });
    it("works on France with longitude estimate", () => {
        expect(aStar(franceDistance as Map<[string, Intercity][]>,
                          longitudeEstimate,
                          'rennes',
                          'avignon')).toEqual(
                              [['rennes', 'paris', 'dijon', 'lyon', 'avignon'], 13]);
    });
});
