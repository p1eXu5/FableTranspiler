Feature: File Processing

Scenario: React types should be ignored
	Given a list of statements in test1.ts file
		* import * as React from 'react';
		* export interface ReactScrollLinkProps{
        *    to: string,
		* }
		* export type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;
		* export default class Link extends React.Component<LinkProps>{}
	When file is parsing
	Then there are 
