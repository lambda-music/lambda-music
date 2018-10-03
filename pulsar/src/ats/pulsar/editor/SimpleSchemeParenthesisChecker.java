package ats.pulsar.editor;

import java.util.regex.Pattern;

import ats.pulsar.editor.SimpleSchemeParser.DefaultModeFactory;
import ats.pulsar.editor.SimpleSchemeParser.ModeFactory;
import ats.pulsar.editor.SimpleSchemeParser.ModeType;
import ats.pulsar.editor.SimpleSchemeParser.ParseDirection;
import ats.pulsar.editor.SimpleSchemeParser.ParserState;

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
