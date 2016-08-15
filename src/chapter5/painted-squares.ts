
import { match } from "../chapter3/match";
import { Map, findKey } from "../util";
export enum Pattern { None, ST, HA, GR, BX };
type Piece = Pattern[];
type PieceName = string;
export enum Orientation { S, E, N, W };
type Placement = [PieceName, Orientation];
type State = Placement[];

const boxWidth = 2;
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
export function orient([piece, orientation]: Placement): Piece {
    return rotateList(pattern[piece], orientation);
}
export function matchNorth(trial: Piece, state: State) {
    return trial[Orientation.N] === orient(state[boxWidth - 1])[Orientation.S];
}
export function matchWest(trial: Piece, state: State) {
    return trial[Orientation.W] === orient(state[0])[Orientation.E];
}
