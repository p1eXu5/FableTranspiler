Feature: File Processing

	Rule: Parsing of referenced module is running automatically

		Background: 
			Given a library located in "C:\Foo"

		Example: Of storing types of not parsed importing module
			Given a "C:\Foo\bar.d.ts" file with content:
				"""
				export interface BarProps{
				   to: string;
				}
				"""
			And a "C:\Foo\foo.d.ts" file with content:
				"""
				import * as Bar from './bar';

				export interface FooProps extends Bar.BarProps {
					name: string;
					id?: string | undefined;
				}
				"""
			When user is choose 
			Then the BarProps type is stored
