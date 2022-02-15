Feature: Parse file

	Scenario: Parsing file without imports and without exports
		Given a "z:\test.d.ts" file with content:
			"""
			export interface Foo {
				to: string;
			}
			"""
		When the file is parsing
		Then an tree with single node is produced
		And the Foo statement is stored
