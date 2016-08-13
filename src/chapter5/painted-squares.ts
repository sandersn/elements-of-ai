
import { match } from "../chapter3/match";
import { Map, findKey } from "../util";
export enum Pattern { ST, HA, GR, BX };
type Piece = Pattern[];
export enum Orientation { S, E, N, W };
type Placement = [string, Orientation];
type State = Placement[];
// this is just an example puzzle I guess
const pattern: Map<Piece> = {
    p1: [Pattern.ST, Pattern.HA, Pattern.GR, Pattern.ST],
    p2: [Pattern.BX, Pattern.ST, Pattern.HA, Pattern.BX],
    p3: [Pattern.ST, Pattern.HA, Pattern.BX, Pattern.GR],
    p4: [Pattern.GR, Pattern.GR, Pattern.HA, Pattern.BX],
}
export function rotateList<T>(l: T[], n: number) {
    const offset = l.length - n;
    return l.slice(offset).concat(l.slice(0, offset));
}
export function orient([piece, orientation]: Placement) {
    return rotateList(pattern[piece], orientation);
}
