declare module "jscons" {
    class Cons {
        static from(xs: any[]): Cons;
        static instanceOf(cons: any): cons is Cons;

        constructor(head: any, tail: Cons);
        head(): any;
        tail(): Cons;
    }
    export = Cons;
}
