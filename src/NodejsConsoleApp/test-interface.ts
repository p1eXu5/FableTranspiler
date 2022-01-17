export interface IFoo {
    onDo?(): void;
    duration?: number | string | ((distance: number) => number) | undefined;
}


class FooExecutor {
    constructor() { }

    do(foo: IFoo) {
        foo.onDo?.();
    }
}


export function execute(foo: IFoo) {
    foo.onDo?.();
    if (foo.duration !== undefined) {
        if (typeof foo.duration === "function") {
            console.log(foo.duration(5));
        }
        else {
            console.log(foo.duration);
        }
    }
}