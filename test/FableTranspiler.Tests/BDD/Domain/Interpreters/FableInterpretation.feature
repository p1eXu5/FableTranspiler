Feature: DTS to Fable interpretation

	Scenario: Module is importing react module and exporting component
		Given a z:\Link.d.ts file with content:
			"""
			import * as React from 'react';

            export interface ReactScrollLinkProps {
                to: string;
            }

            export type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;

            export default class Link extends React.Component<LinkProps> {}
			"""
		When the z:\Link.d.ts file is parsing
		Then fs file content is producing:
			"""

			"""