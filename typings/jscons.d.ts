declare module "jscons" {
    class Cons<T> {
        static from<U>(xs: U[]): Cons<U>;
        static instanceOf<U>(cons: any): cons is Cons<U>;

        constructor(head: T, tail: Cons<T>);
        head(): T;
        tail(): Cons<T>;
    }
    export = Cons;
}
