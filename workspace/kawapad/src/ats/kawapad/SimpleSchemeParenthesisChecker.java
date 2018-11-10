/*
 * Kawapad written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Kawapad is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Kawapad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Kawapad.  If not, see <https://www.gnu.org/licenses/>.
 */

package ats.kawapad;

import java.util.regex.Pattern;

import ats.kawapad.SimpleSchemeParser.DefaultModeFactory;
import ats.kawapad.SimpleSchemeParser.ModeFactory;
import ats.kawapad.SimpleSchemeParser.ModeType;
import ats.kawapad.SimpleSchemeParser.ParseDirection;
import ats.kawapad.SimpleSchemeParser.ParserState;

/**
 *  October 3, 2018 at 9:52:22 PM
 */
public class SimpleSchemeParenthesisChecker {
	static final boolean DEBUG = false; 
	public static void lookupParenthesisProc( ParserState parserState ) {
		boolean started = false;
		boolean result = false;
//		ArrayDeque<Mode> stack = new ArrayDeque<>();
		ModeFactory factory = new DefaultModeFactory();
		
		// Initialize the stack.
		parserState.getStack().push( factory.create( null, ModeType.PARENTHESIS, parserState ) );

		for(;; parserState.getIterator().next() ) {
			if ( DEBUG )
				System.out.print( parserState.getIterator().getCurrentChar() );

			if ( parserState.getIterator().getCurrentChar() == 0 ) {
				break;
			}

			if ( ! parserState.getStack().peek().process( parserState ) ) {
				break;
			}
			if ( started ) {
				if ( parserState.getStack().size() <= 1 ) {
					result = true;
					break;
				}
			} else {
				if ( parserState.getStack().size() <= 1 ) {
				} else {
					started = true;
				}
			}
		}

		if ( DEBUG )
			System.out.println();
		parserState.setFound( result );
	}

	public static String swapEscapeSequence( String s ) {
		return Pattern.compile( "(\\\\)(.)" ).matcher(s).replaceAll( "$2$1" );
	}

	public static ParserState lookupParenthesis( String string, int index ) {
		SimpleSchemeParser.ParserState parserState = null;
		
		/*
		 * If index is in the position beyond the length, do nothing. Note that the
		 * default value of 'found' property on the object is always false.
		 */
		if (  string.length() <= index )
			return new SimpleSchemeParser.ParserState( string, index, ParseDirection.FORWARD );
		
		// added (Sun, 07 Oct 2018 17:00:57 +0900)
		if ( index < 0 )
			return new SimpleSchemeParser.ParserState( string, index, ParseDirection.FORWARD );

		switch ( string.charAt(index) ) {
			case '(' :
				// LOGGER.info("(");
				parserState = new SimpleSchemeParser.ParserState( string, index, ParseDirection.FORWARD );
				lookupParenthesisProc( parserState  );
				return parserState;
			case ')' : {
				// LOGGER.info(")");
				parserState = new SimpleSchemeParser.ParserState( string, index, ParseDirection.BACKWARD );
				String original = parserState.getIterator().getString();
				parserState.getIterator().setString( swapEscapeSequence( original ) );
				lookupParenthesisProc( parserState );
				parserState.getIterator().setString( original );
				return parserState;
			}
			
			default :
				/*
				 * If the current character is a character other than parentheses, do nothing.
				 * Return a plain parser state object where the default value of 'found'
				 * property is always false. 
				 */
				return new SimpleSchemeParser.ParserState( string, index, ParseDirection.FORWARD );
		}
	}

	public static void main(String[] args) {
		String s = "     ( hello foo ( hello \\) ) \"fgsfdsg\" ()123456)abc";
		ParserState parserState = SimpleSchemeParenthesisChecker.lookupParenthesis( s, 48 );

		System.out.println( parserState.getIterator().getString() );
		
		if ( parserState.isFound() ) {
			System.out.println( SimpleSchemeIndentChanger.fillStr( ' ', parserState.getIterator().getInitialIndex() ) + '^' );
			System.out.println( SimpleSchemeIndentChanger.fillStr( ' ', parserState.getIterator().getIndex() ) + '^' );
		} else {
			System.out.println( "NOT FOUND" );
		}
	}
}
