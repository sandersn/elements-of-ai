
import { match } from "../chapter3/match";
import { Map, findKey, concatMap, cross } from "../util";
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
export function orient([pieceName, orientation]: Placement): Piece {
    return rotateList(pattern[pieceName], orientation);
}
export function matchNorth(trial: Piece, state: State) {
    return trial[Orientation.N] === orient(state[boxWidth - 1])[Orientation.S];
}
export function matchWest(trial: Piece, state: State) {
    return trial[Orientation.W] === orient(state[0])[Orientation.E];
}
/**
 * I added some comments specific to the 2x2 puzzle although this works for any size puzzle
 * (orient takes care of retrieving the correct puzzle to match)
 */
export function sidesOk(placement: Placement, currentState: State) {
    const trial = orient(placement);
    const len = currentState.length;
    if (len === 0) { // first piece
        return true; // no match required
    }
    else if (len % boxWidth === 0) { // third piece
        return matchNorth(trial, currentState); // match up to first piece
    }
    else if (len < boxWidth) { // second piece
        return matchWest(trial, currentState); // match left to first piece
    }
    else { // fourth piece
        // match up to second piece, left to third piece
        return matchNorth(trial, currentState) && matchWest(trial, currentState);
    }
}
export function solveSquares(state: State, unusedPieces: PieceName[]): State[] {
    if (unusedPieces.length === 0) {
        return [state];
    }
    else {
        return concatMap(cross(holdouts(unusedPieces), [0, 1, 2, 3]),
                         ([[piece, rest], dir]) => tryOrientation(dir, piece, state, rest));
    }
}
export function tryOrientation(dir: Orientation, piece: PieceName, state: State, rest: PieceName[]) {
    const placement: Placement = [piece, dir];
    return sidesOk(placement, state) ? solveSquares([placement].concat(state), rest) : [];
}
export function holdouts<T>(l: T[]): [T, T[]][] {
    return l.map((x, i) => [x, l.slice(0, i).concat(l.slice(i+1))] as [T, T[]]);
}
export function solve() {
    return solveSquares([], Object.keys(pattern));
}
export function show(solution: State, count: number) {
    const s = solution.map(([name, dir]) => `${name}: ${Orientation[dir]}`).join(', ')
    console.log(`Solution ${count}: {${solution}}`);
}
export function showSolution() {
    solveSquares([], Object.keys(pattern)).forEach(show);
}
