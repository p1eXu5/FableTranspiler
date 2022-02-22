Feature: DTS to Fable interpretation

	Scenario: Importing the Link module (crop)
		Given a z:\Link.d.ts file with content:
			"""
			import * as React from 'react';

            export interface ReactScrollLinkProps {
                to: string;
				spy?: boolean | undefined;
				smooth?: boolean | string | undefined;
				duration?: number | string | ((distance: number) => number) | undefined;
            }

            export type LinkProps = ReactScrollLinkProps & React.HTMLProps<HTMLButtonElement>;

            export default class Link extends React.Component<LinkProps> {}
			"""
		When the z:\Link.d.ts file is parsing with ReactScroll namespace
		Then fs file content is producing:
			"""
			namespace ReactScroll

			open Fable.Core
			open Fable.Core.JsInterop
			open Fable.React.Props

			type ReactScrollLinkProps =
			    | To of string
			    | Spy of bool option
			    | Smooth of U2<bool, string> option
			    | Duration of U3<float, string, (float -> float)> option
			    interface IHTMLProp

			type LinkProps =
			    | To of string
			    | Spy of bool option
			    | Smooth of U2<bool, string> option
			    | Duration of U3<float, string, (float -> float)> option
			    interface IHTMLProp

			module Link =
			    open Fable.React

			    let inline link props children = domEl "Link" props children
			"""


	Scenario: Importing the Element module (crop)
		Given a z:\Element.d.ts file with content:
			"""
			import * as React from 'react';

			export interface ElementProps extends React.HTMLProps<HTMLDivElement> {
				name: string;
				id?: string | undefined;
			}

			export default class Element extends React.Component<ElementProps> {}
			"""
		When the z:\Element.d.ts file is parsing with ReactScroll namespace
		Then fs file content is producing:
			"""
			namespace ReactScroll

			open Fable.React.Props

			type ElementProps =
			    | Name of string
			    | Id of string option
			    interface IHTMLProp

			module Element =
			    open Fable.React

			    let inline element props children = domEl "Element" props children
			"""
