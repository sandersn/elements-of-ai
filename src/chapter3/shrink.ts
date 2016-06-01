import { Map, chooseRandom } from "../util";
import { match } from "./match";
const youMe: Map<string> = {
    i: 'you',
    me: 'you',
    you: 'me',
    my: 'your',
    your: 'my',
    yours: 'mine',
    mine: 'yours',
    am: 'are'
}
    
function convertYouMe(s: string[]): string[] {
    return s.map(word => youMe[word.toLowerCase()] || word);
}
function read(): string[] {
    return "I don't know to read from stdin in node".split(' ');
}
function chooseFirst<T,U>(f: (t: T) => U, keys: T[]): [T, U] {
    for (const key of keys) {
        if (key) {
            const value = f(key);
            if (value) {
                return [key, value];
            }
        }
    }
}
function parsePattern(s: string): any[] {
    // TODO: Json.parse here instead. No idea what the function is called.
    return eval(s);
}
function wWord(): string {
    return chooseRandom(['why', 'where', 'when', 'what', 'which', 'how']);
}
function punt(): string {
    return chooseRandom(['Please go on.',
                         'Tell me more.',
                         'I see.',
                         "What does that indicate.",
                         "But why be concerned about it?",
                         "Just tell me how you feel."]);
                         

}
function mentions(word: string, s: string[]): boolean {
    return s.indexOf(word) > -1;
}
const knowledge: Map<((d: Map<any>) => void)> = {
    "['bye']": _ => "goodbye",
    "['you', 'are', ['*', 'x']]": d => console.log(`Please tell me ${wWord()} you are ${d['x']}.`)
};
export function shrink() {
    console.log("Welcome to my sofa!");
    while (true) {
        const s = convertYouMe(read());
        const result = chooseFirst(x => match(parsePattern(x), s), Object.keys(knowledge));
        if (result) {
            const [pattern, matches] = result;
            const result2 = knowledge[pattern](matches);
            if (result2) { // if the pattern returns instead of printing we are done
                return result2;
            }
        }
        else {
            if (mentions('dream', s)) {
                console.log('For dream analysis see Freud.');
            }
            else if (mentions('love', s)) {
                console.log('All is fair in love and war.');
            }
            else if (mentions('no', s)) {
                console.log("Don't be so negative.");
            }
            else if (mentions('maybe', s)) {
                console.log("Be more decisive!");
            }
            else if (mentions('you', s)) {
                console.log(s.join(' '));
            }
            else {
                console.log(punt());
            }
        }
    }
}
