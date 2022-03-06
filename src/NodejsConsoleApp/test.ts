export { IFoo, execute } from './test-interface';

export const Greeter = (name: string) => `Hello ${name}`;

export function greeter2(name: string) {
    return `Hello ${name}`;
}

export function unitParameter(options: { foo: string | number }): (v: number) => number {
    if (typeof options.foo === "string") {
        return (v: number) => v * 2
    }
    else if (typeof options.foo === "number") {
        return (v: number) => v * 0.5
    }
}