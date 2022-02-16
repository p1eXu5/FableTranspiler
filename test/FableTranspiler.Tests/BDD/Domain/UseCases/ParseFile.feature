Feature: Parse file

	Scenario: Parsing single file without imports and without exports
		Given a z:\test.d.ts file with content:
			"""
			export interface Foo {
				to: string;
			}
			"""
		When the z:\test.d.ts file is parsing
		Then a tree with single z:\test.d.ts root node is produced
		And the Foo statement from z:\test.d.ts file is stored


	Scenario: Parsing multiple files with imports and exports
		Given a z:\test-foo.d.ts file with content:
			"""
			export interface Foo {
				to: string;
			}
			"""
		And an another z:\test-foo-props.d.ts file with content:
			"""
			import { Foo } from "./test-foo";

			export type FooProps =
				Foo & {
					container?: HTMLElement | undefined;
				};
			"""
		When the z:\test-foo-props.d.ts file is parsing
		Then a tree has z:\test-foo-props.d.ts root node importing:
			* z:\test-foo.d.ts
		And a z:\test-foo.d.ts node is imported to:
			* z:\test-foo-props.d.ts
		And the Foo statement from z:\test-foo.d.ts file is stored
		And the FooProps statement from z:\test-foo-props.d.ts file is stored
