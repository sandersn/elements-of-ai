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
const verbs: Map<number> = {
    go: 0,
    have: 0,
    be: 0, 
    'try': 0,
    eat: 0,
    take: 0,
    help: 0,
    make: 0,
    get: 0,
    jump: 0,
    write: 0,
    type: 0,
    fill: 0,
    put: 0,
    turn: 0,
    compute: 0,
    think: 0,
    drink: 0,
    blink: 0,
    crash: 0,
    crunch: 0,
    add: 0
};
    
    
function convertYouMe(s: string[]): string[] {
    return s.map(word => youMe[word.toLowerCase()] || word);
}
function find<T,U>(f: (t: T) => U, l: T[]): T {
    for (const x of l) {
        if (f(x)) {
            return x;
        }
    }
}
const wWord = ['why', 'where', 'when', 'what', 'which', 'how'];
function chooseWWord(): string {
    return chooseRandom(wWord);
}
function isWWord(s: string): boolean {
    return wWord.indexOf(s) > -1;
}
function isModalWord(s: string) {
    return ['do', 'can', 'should', 'would'].indexOf(s) > -1;
}
function punt(): string {
    return chooseRandom(['Please go on.',
                         'Tell me more.',
                         'I see.',
                         "What does that indicate?",
                         "But why be concerned about it?",
                         "Just tell me how you feel."]);
}
function mentions(word: string, s: string[]): boolean {
    return s.indexOf(word) > -1;
}
type Literal = string
type Pattern = [string, string]
const knowledge: [(Literal | Pattern)[], (d: Map<any>) => void][] = [
    [['bye'], _ => "goodbye"],
    [['you', 'are', ['*', 'x']], d => console.log(`Please tell me ${chooseWWord()} you are ${d['x']}.`)],
    [['you', 'have', ["*", 'x']], d => console.log(`How long have you had ${d['x'].join(' ')}`)],
    [['you', 'feel', ["*", 'x']], d => console.log("I sometimes feel the same way.")],
    [['because', ["*", 'x']], d => console.log("Is that really the reason?")],
    [[], d => console.log("Please say something.")],
    [['yes', ["*", 'x']], d => console.log("How can you be so sure?")],
    [['me', 'are', ["*", 'x']], d => console.log(`Oh yeah I am ${d['x'].join(' ')}`)],
    [[['verb', 'v'], ["*", 'x']], d => console.log(`Why do you want me to ${d['v']} ${d['x'].join(' ')}`)],
    [[['isWWord', 'w'], ["*", 'x']], d => console.log(`You tell me ${d['w']} ${d['x'].join(' ')}`)],
    [[['isModalWord', 'w'], 'me', ["*", 'x']], d => console.log(`Perhaps I ${d['w']} #{d['x'].join(' ')}`)],
    [['do', 'me', 'think', ["*", 'x']], d => console.log("I think you should answer that for yourself.")]
];
export function shrink(read: () => string) {
    console.log("Welcome to my sofa!");
    while (true) {
        const sentence = convertYouMe(read().toLowerCase().split(' '));
        const production = find(([pattern,_]) => match(pattern, sentence), knowledge);
        if (production) {
            const [pattern,rule] = production;
            const result = rule(match(pattern, sentence))
            if (result) {
                console.log(result);
                return;
            }
        }
        else {
            if (mentions('dream', sentence)) {
                console.log('For dream analysis see Freud.');
            }
            else if (mentions('love', sentence)) {
                console.log('All is fair in love and war.');
            }
            else if (mentions('no', sentence)) {
                console.log("Don't be so negative.");
            }
            else if (mentions('maybe', sentence)) {
                console.log("Be more decisive!");
            }
            else if (mentions('you', sentence)) {
                console.log(sentence.join(' '));
            }
            else {
                console.log(punt());
            }
        }
    }
}
export function autoShrink() {
    let line = -1;
    const lines = [
        "Hello",
        "I have a small problem",
        "Ever since my girlfriend discovered my true love",
        "I see",
        "Yes",
        "Because I understand your comment",
        "Perhaps not",
        "She wants me to give up computers",
        "You got it",
        "I can't see you anymore",
        "No but",
        "But I will dream of you",
        "Bye"
    ];
    function read() {
        line++;
        console.log("> " + lines[line]);
        return lines[line];
    }
    shrink(read);
}
