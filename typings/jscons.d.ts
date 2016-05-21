declare module "jscons" {
    interface Cons {
        head(): any;
        tail(): Cons;
    }
    interface Cons_static {
        new(head: any, tail: Cons): Cons;
        from(xs: any[]): Cons;
        instanceOf(cons: any): cons is Cons;
    }
    var Cons: Cons_static;
    export = Cons;
}
