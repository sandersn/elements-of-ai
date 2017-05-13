/// <reference path="../../typings/jasmine.d.ts"/>
import { equal, Map } from "../util";
import { Node } from "./infnet"
describe("prove", () => {
    it("passes the example", () => {
        expect(prove(parseProver("(a & (not b)) -> a"))).toEqual("VALID");
    });
});

