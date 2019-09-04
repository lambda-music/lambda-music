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
import java.util.regex.Pattern;

public class SimpleSchemeParenthesisParser {
    static final boolean DEBUG = false;
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
        public boolean process( ParserState state) {
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
        private CharSequence string;
        private int initialIndex;
        private int index=0;
        public CharIterator(CharSequence s, int index, ParseDirection direction ) {
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
        public CharSequence substring( int beginIndex, int endIndex ) {
            return string.subSequence( beginIndex, endIndex);
        }
        public CharSequence getString() {
            return string;
        }
        public CharSequence setString( CharSequence string ) {
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
        public ParserState(CharSequence  string, int index, ParseDirection direction ) {
            super();
            this.iterator = new CharIterator( string, index, direction );
        }
        public void setFound(boolean found) {
            this.found = found;
        }
    }

    public static ParserState lookupParenthesis( CharSequence string, int index ) {
        ParserState parserState = null;
        
        /*
         * If index is in the position beyond the length, do nothing. Note that the
         * default value of 'found' property on the object is always false.
         */
        if (  string.length() <= index )
            return new ParserState( string, index, ParseDirection.FORWARD );
        
        // added (Sun, 07 Oct 2018 17:00:57 +0900)
        if ( index < 0 )
            return new ParserState( string, index, ParseDirection.FORWARD );
    
        switch ( string.charAt(index) ) {
            case '(' :
                // LOGGER.info("(");
                parserState = new ParserState( string, index, ParseDirection.FORWARD );
                lookupParenthesisProc( parserState  );
                return parserState;
            case ')' : {
                // LOGGER.info(")");
                parserState = new ParserState( string, index, ParseDirection.BACKWARD );
                CharSequence original = parserState.getIterator().getString();
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
                return new ParserState( string, index, ParseDirection.FORWARD );
        }
    }

    public static void lookupParenthesisProc( ParserState parserState ) {
            boolean started = false;
            boolean result = false;
    //      ArrayDeque<Mode> stack = new ArrayDeque<>();
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

    public static String swapEscapeSequence( CharSequence s ) {
        return Pattern.compile( "(\\\\)(.)" ).matcher(s).replaceAll( "$2$1" );
    }

    public static final int lookupCorrespondingParenthesis( CharSequence text, int position ) {
        ParserState parserState = lookupParenthesis( text, position );
        if ( parserState.isFound() ) {
            return parserState.getIterator().getIndex();
        } else {
            return -1;
        }
    }
    public static void main(String[] args) {
        String s = "     ( hello foo ( hello \\) ) \"fgsfdsg\" ()123456)abc";
        ParserState parserState = lookupParenthesis( s, 48 );

        System.out.println( parserState.getIterator().getString() );
        
        if ( parserState.isFound() ) {
            System.out.println( SimpleSchemeIndentChanger.fillStr( ' ', parserState.getIterator().getInitialIndex() ) + '^' );
            System.out.println( SimpleSchemeIndentChanger.fillStr( ' ', parserState.getIterator().getIndex() ) + '^' );
        } else {
            System.out.println( "NOT FOUND" );
        }
    }
}
