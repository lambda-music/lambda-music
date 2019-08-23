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

package kawapad;

import java.util.ArrayDeque;
import java.util.ArrayList;

public class SimpleSchemeParser {
	public static enum ParseDirection { FORWARD,BACKWARD }
	public static enum ModeType { PARENTHESIS, STRING, ESCAPESEQUENCE }
	public static abstract class ModeFactory {
		public abstract Mode create( Mode parentMode, ModeType type, ParserState state );
	}
	public static class TokenInfo {
		public int lineStartIndex;
		public int lineColumnIndex;
		public TokenInfo(int lineStartIndex, int lineColumnIndex) {
			this.lineStartIndex = lineStartIndex;
			this.lineColumnIndex = lineColumnIndex;
		}
	}
	public static abstract class Mode {
		protected final Mode parent;
		protected final ModeFactory factory;
		protected final ParserState state;
		public Mode(ParserState state, Mode parent, ModeFactory factory ) {
			this.parent = parent;
			this.factory = factory;
			this.state = state;
		}
		protected StringBuffer sb = new StringBuffer();
		protected ArrayList<String>    tokenList = new ArrayList<>();
		protected ArrayList<TokenInfo> tokenInfoList = new ArrayList<>();
		protected void breakProc() {
			if ( 0< sb.length() ) {
				tokenList.add( sb.toString() );
				sb.setLength(0);
			}
		}
		protected void appendProc( char c ) {
			if ( sb.length() == 0 ) {
				int lineStart = SimpleSchemeIndentChanger.lookupLineStart( state.getIterator().getString(), state.getIterator().getIndex() );
				int indentSize = state.getIterator().getIndex() - lineStart;
				this.tokenInfoList.add( new TokenInfo( lineStart , indentSize ));
			}
			sb.append(c);
		}

		public void procChar(char c) {
			if ( c == 0 ) {
				breakProc();
			} else {
				appendProc(c);
			}
		}

		public abstract boolean process( ParserState state );
		
		private Object userObject = null;
		public Object getUserObject() {
			return userObject;
		}
		public void setUserObject(Object userObject) {
			this.userObject = userObject;
		}
	}
	
	public static class DefaultModeFactory extends ModeFactory {
		@Override
		public Mode create( Mode parentMode, ModeType type, ParserState state ) {
			switch ( type ) {
				case PARENTHESIS :
					return new ParenthesisMode( state, parentMode, this );
				case STRING:
					return new StringMode( state, parentMode, this );
				case ESCAPESEQUENCE:
					return new EscapeSequenceMode( state, parentMode, this );
				default:
					break;
			}
			throw new Error( "this will never happen" );
		}
	}
	
	public static class ParenthesisMode extends Mode {
		@Override
		public void procChar( char c ) {
			switch ( c ) {
				case ' ':
				case '\t':
				case '\n':
				case '\r':
					breakProc();
					break;
				default:
					appendProc(c);
			}
		}
		
		public ParenthesisMode(ParserState state, Mode parentMode, ModeFactory factory ) {
			super(state, parentMode, factory);
		}

		public boolean process( ParserState state ) {
			char c = state.getIterator().getCurrentChar();
			switch (  state.getIterator().getDirection() ) {
				case FORWARD :
					switch ( c ) {
						case '(' :
							procChar('\u0000');
							state.getStack().push( factory.create( state.getStack().peek(), ModeType.PARENTHESIS, state ) );
							break;
						case ')':
							procChar('\u0000');
							state.getStack().pop();
							break;
						case '"' :
							procChar('\u0000');
							state.getStack().push( factory.create( state.getStack().peek(), ModeType.STRING, state ) );
							break;
						case '\\' :
							state.getStack().push( factory.create( state.getStack().peek(), ModeType.ESCAPESEQUENCE, state ) );
							break;
						default :
							procChar(c);
							;
					}
					break;
				case BACKWARD :
					switch ( c ) {
						case ')':
							procChar('\u0000');
							state.getStack().push( factory.create( state.getStack().peek(), ModeType.PARENTHESIS, state ) );
							break;
						case '(' :
							procChar('\u0000');
							state.getStack().pop();
							break;
						case '"' :
							procChar('\u0000');
							state.getStack().push( factory.create( state.getStack().peek(), ModeType.STRING, state ) );
							break;
						case '\\' :
							procChar('\u0000');
							state.getStack().push( factory.create( state.getStack().peek(), ModeType.ESCAPESEQUENCE, state ) );
							break;
						default :
							procChar(c);
							;
					}
					break;
			}

			return true;
		}
	}

	public static class StringMode extends Mode {
		public StringMode( ParserState state, Mode parentMode, ModeFactory factory ) {
			super( state, parentMode, factory );
		}
		public boolean process( SimpleSchemeParser.ParserState state) {
			char c = state.getIterator().getCurrentChar();
			switch ( c ) {
				case '"' :
					procChar((char)0 );
					state.getStack().pop();
					break;
				case '\\' :
					state.getStack().push( factory.create( state.getStack().peek(), ModeType.ESCAPESEQUENCE, state ) );
					break;
				default :
					procChar( c );
			}
			return true;
		}
	}
	public static class EscapeSequenceMode extends Mode {
		public EscapeSequenceMode( ParserState state, Mode parentMode, ModeFactory factory ) {
			super( state, parentMode, factory );
		}
		public boolean process( ParserState state) {
			char c = state.getIterator().getCurrentChar();
			switch ( c ) {
				default :
					procChar( c );
					state.getStack().pop();
					break;
			}
			return true;
		}
		@Override
		public void procChar(char c) {
			parent.procChar(c);
		}
	}

	public static class CharIterator {
		private ParseDirection direction;
		private String string;
		private int initialIndex;
		private int index=0;
		public CharIterator(String s, int index, ParseDirection direction ) {
			super();
			this.string = s;
			this.index = index;
			this.initialIndex = index;
			this.direction = direction;
		}
		public char right() {
			this.index ++;
			return getCurrentChar();
		}
		public char left() {
			this.index --;
			return getCurrentChar();
		}
		public char getCurrentChar() {
			if ( 0<=this.index && this.index < string.length() ) {
				return string.charAt( this.index );
			} else {
				return 0;
			}
		}
		public int getIndex() {
			return index;
		}
		public int getInitialIndex() {
			return initialIndex;
		}
		public String substring( int beginIndex, int endIndex ) {
			return string.substring(beginIndex, endIndex);
		}
		public String getString() {
			return string;
		}
		public String setString( String string) {
			this.string = string;
			return string;
		}
		public void next() {
			switch ( direction ) {
				case FORWARD :
					this.right();
					break;
				case BACKWARD : 
					this.left();
					break;
			}
		}
		public ParseDirection getDirection() {
			return direction;
		}
	}

	public static class ParserState {
		private boolean found=false;
		private ArrayDeque<Mode> stack = new ArrayDeque<>();
		public ArrayDeque<Mode> getStack() {
			return stack;
		}
		private CharIterator iterator;
		public CharIterator getIterator() {
			return iterator;
		}
		public ParserState(CharIterator iterator) {
			super();
			this.iterator = iterator;
		}
		public boolean isFound() {
			return found;
		}
		public ParserState(String string, int index, ParseDirection direction ) {
			super();
			this.iterator = new CharIterator( string, index, direction );
		}
		public void setFound(boolean found) {
			this.found = found;
		}
	}

}
