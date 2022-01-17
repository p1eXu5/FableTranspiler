export { IFoo, execute } from './test-interface';

export const Greeter = (name: string) => `Hello ${name}`;

export function greeter2(name: string) {
    return `Hello ${name}`;
}