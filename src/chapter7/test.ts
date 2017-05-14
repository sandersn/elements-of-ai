/// <reference path="../../typings/jasmine.d.ts"/>
import { test } from "./infnet"
describe("updateNodes", () => {
    it("updates non-input nodes", () => {
        expect(test()).toEqual(null);
    });
});

