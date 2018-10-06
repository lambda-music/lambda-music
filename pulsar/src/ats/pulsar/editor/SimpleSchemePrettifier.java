package ats.pulsar.editor;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * October 3, 2018 at 9:52:28 PM
 * @author ats
 */
public class SimpleSchemePrettifier {
	static enum TokenType { BEGIN, END, ID }
	static class Token {
		TokenType type;
		String content;
		int index;
		int lineNo;
		public Token(TokenType type, String content, int index, int lineNo ) {
			super();
			this.type = type;
			this.content = content;
			this.index = index;
			this.lineNo = lineNo;
		}
		@Override
		public String toString() {
			return String.format("[%s %d \"%s\"]", type, index, content  );
		}
	}
	static class Tokenized extends ArrayList<Token> {
		void shift( int i ) {
			for ( Token t : this ) {
				t.index += i;
			}
		}
	}

	public static class Tokenizer {
		private static final int UNKNOWN = Integer.MIN_VALUE;
		int lineNo;
		int beginIndex = UNKNOWN;
		Tokenized tokenized = new Tokenized();
		StringBuilder sb = new StringBuilder();
		public Tokenizer(int lineNo) {
			this.lineNo = lineNo;
		}
		boolean addToken( TokenType type, String content, int index ) {
			return tokenized.add( new Token( type, content, index, lineNo ) );
		}
		private void appendProc( char c, int index ) {
			if ( beginIndex == UNKNOWN )
				beginIndex = index;
			sb.append(c);
		}
		void breakProc(int index) {
			if ( 0 < sb.length() ) {
				addToken( TokenType.ID, sb.toString(), beginIndex );
				sb.setLength(0);
				beginIndex = UNKNOWN;
			}
		}
	}
	
	
	public static Tokenized tokenize( String text, int lineNo ) {
		Tokenizer tokenizer = new Tokenizer( lineNo );
		for ( int i=0; i<text.length(); i++ ) {
			switch ( text.charAt(i) ) {
				case '(' :
					tokenizer.breakProc(i);
					tokenizer.addToken( TokenType.BEGIN, "(", i );
					break;
				case ')' :
					tokenizer.breakProc(i);
					tokenizer.addToken( TokenType.END, ")", i );
					break;
				case '\\' :
					i++;
					tokenizer.appendProc( text.charAt( i ), i);
					break;
				case '\t':
				case '\n':
				case '\r':
				case ' ' :
					tokenizer.breakProc(i);
					break;
				default :
					tokenizer.appendProc( text.charAt( i ), i );
					break;
			}
		}
		tokenizer.breakProc(text.length() );
		return tokenizer.tokenized;
	}
	
	public static class Level {
		Tokenized tokenized = new Tokenized();
		void addToken( Token t  ) {
			tokenized.add( t );
		}
	}
	
	public static String prettify( Collection<String> lispWords, String text ) {
		/*
		 * In this function, we always presume that ( lines.length == tokenizedLines ).
		 */
		String[] lines = text.split( "\n" );
		Tokenized[] tokenizedLines = new Tokenized[ lines.length ];
		for ( int i=0; i<lines.length ;i++ ) {
			tokenizedLines[i] = tokenize( lines[i], i );
		}
		
		ArrayDeque<Level> stack = createStack();

		/*
		 *  1. Process all tokenized lines. 
		 */
		for ( int i=0; i<tokenizedLines.length ;i++ ) {
			Tokenized tokenized = tokenizedLines[i] ;
			
			/*
			 *  2. Determine the indent size. 
			 *     This block is only executed after processed the first line.
			 *     See calculateIndexLevel() function. 
			 */
			if ( 0<i) {
				if ( 1<stack.size() ) {
					int index = calculateIndentLength(lispWords, stack);
					
					if ( 0 <=index  ) {
						int diff = index - tokenizedLines[i].get(0).index;
						for ( int j=i; j<tokenizedLines.length; j++ ) {
							tokenizedLines[j].shift(diff);
						}
					}
				}
			}


			/*
			 * 3. Scan all the tokens and "execute" them to generate a simplified version of
			 *    the parse-tree.
			 */
			semiExecute(stack, tokenized);
		}
		
		/*
		 * 4. 
		 * 
		 */
		for ( int i=0; i<tokenizedLines.length ;i++ ) {
			Tokenized tokenized = tokenizedLines[i] ;
			if ( 0 < tokenized.size() ) {
				lines[i] = SimpleSchemeIndentChanger.changeIndentAbsolute( lines[i], tokenized.get(0).index );
			}
		}
		
		return String.join( "\n", lines ); 
	}

	public static ArrayDeque<Level> createStack() {
		ArrayDeque<Level> stack = new ArrayDeque<>();
		// Add the base level.
		stack.push( new Level() );
		return stack;
	}


	public static int calculateIndentLength( Collection<String> lispWords, ArrayDeque<Level> stack ) {
		int length = -1;
		Level level = stack.peek();
		if ( 0 == level.tokenized.size()  ) {
			// This means the number of parentheses exceeds our expectation so ignore it.
		} else if ( 1 == level.tokenized.size()  ) {
			// There is the only one parenthesis or the only one token. 
			length = level.tokenized.get(0).index + 1;
		} else if ( 2 == level.tokenized.size()  ) {
			length = level.tokenized.get(1).index;
		} else if ( 3 <= level.tokenized.size()  ) {
			if ( lispWords.contains( level.tokenized.get(1).content ) ) {
				length = level.tokenized.get(0).index + 2;
			} else {
				length = level.tokenized.get(2).index;
			}
		}
		return length;
	}


	public static void semiExecute(ArrayDeque<Level> stack, Tokenized tokenized) {
		for ( Token t : tokenized ) {
			switch ( t.type ) {
				case BEGIN:
					/*
					 * Note thtat add the token twice; the first addition of the token is a dummy
					 * token which keeps the information about its position of the parenthesis and tells
					 * the information at the outer level which is old.
					 */
					stack.peek().addToken(t); // to the old stack

					/*
					 * Create a new stack level.
					 */
					stack.push( new Level() );

					/*
					 * This is the second addition of the token. This goes to the first element of
					 * the inner level which is new.
					 */
					stack.peek().addToken(t); // to the new stack 
					break;
				case END:
					// Protect the base level and ignore any unnecessary parentheses.
					if ( 1< stack.size() ) {
						stack.peek().addToken(t);
						stack.pop();
					}
					break;
				case ID:
					stack.peek().addToken(t);
					break;
				default:
					break;
			}
		}
	}
	
	
	public static String calculateIndentSizeO(String text, int pos, Collection<String> lispWords ) {
		int beginIndex = SimpleSchemeIndentChanger.lookupLineStart(text, pos );
		int endIndex   = SimpleSchemeIndentChanger.lookupLineEnd(  text, pos );
		String subText = text.substring(beginIndex, endIndex);
		Tokenized tokenizedLine = tokenize( subText, 0 );
		ArrayDeque<Level> stack = createStack();
		semiExecute(stack, tokenizedLine );
		int length = calculateIndentLength( lispWords, stack );
		return SimpleSchemeIndentChanger.fillStr(' ', length );
	}
	
	public static String calculateIndentSize(String text, int pos, Collection<String> lispWords ) {
		String subText = text.substring(0, pos) ;
		String[] lines = subText.split( "\n" );
		Tokenized[] tokenizedLines = new Tokenized[ lines.length ];
		for ( int i=0; i<lines.length ;i++ ) {
			tokenizedLines[i] = tokenize( lines[i], i );
		}
		ArrayDeque<Level> stack = createStack();

		/*
		 *  1. Process all tokenized lines. 
		 */
		for ( int i=0; i<tokenizedLines.length ;i++ ) {
			Tokenized tokenized = tokenizedLines[i] ;
			semiExecute(stack, tokenized);
		}
		
		/*
		 *  2. Determine the indent size. 
		 *     This block is only executed after processed the first line.
		 *     See calculateIndexLevel() function. 
		 */
		if ( 1<stack.size() ) {
			int index = calculateIndentLength(lispWords, stack);
			if ( 0 <=index  ) {
				return SimpleSchemeIndentChanger.fillStr(' ', index );
			}
		}
		return "";
	}


	public static final String[] LISP_WORDS = {
			"defun",
			"define",
			"defmacro",
			"set!",
			"lambda",
			"lambda*",
			"if",
			"case",
			"let",
			"flet",
			"let*",
			"letrec",
			"do",
			"do*",
			"define-syntax",
			"let-syntax",
			"letrec-syntax",
			"destructuring-bind",
			"defpackage",
			"defparameter",
			"defstruct",
			"deftype",
			"defvar",
			"do-all-symbols",
			"do-external-symbols",
			"do-symbols",
			"dolist",
			"dotimes",
			"ecase",
			"etypecase",
			"eval-when",
			"labels",
			"macrolet",
			"multiple-value-bind",
			"multiple-value-call",
			"multiple-value-prog1",
			"multiple-value-setq",
			"prog1",
			"progv",
			"typecase",
			"unless",
			"unwind-protect",
			"when",
			"with-input-from-string",
			"with-open-file",
			"with-open-stream",
			"with-output-to-string",
			"with-package-iterator",
			"define-condition",
			"handler-bind",
			"handler-case",
			"restart-bind",
			"restart-case",
			"with-simple-restart",
			"store-value",
			"use-value",
			"muffle-warning",
			"abort",
			"continue",
			"with-slots",
			"with-slots*",
			"with-accessors",
			"with-accessors*",
			"defclass",
			"defmethod",
			"print-unreadable-object",};


	public static void main(String[] args) {
		ArrayList<Token> result = tokenize( "( HELLO WORLD\\( \\) \\\\FOO \\\\ )", 0 );
		System.out.println( result );

		List<String> lispWords = Arrays.asList("lambda" );
		
		System.out.println( prettify( lispWords, "(lambda()\nhello world \n foo)" ));

		System.out.println( prettify( lispWords, "(lambda args\nhello world \n foo)" ) );
		System.out.println( prettify( lispWords, "(lambda* args\nhello world \n foo)" ) );
		System.out.println( prettify( lispWords, "(lambda*  () aa \nhello world \n foo)" ) );
		System.out.println( prettify( lispWords, "(\n'foo\n 'bar\n 'bum\n )" ) );
		System.out.println( prettify( lispWords, "('foo\n 'bar\n 'bum\n )" ) );
		System.out.println( prettify( lispWords, "(\n(\n(\n(\n(\n)\n)\n)\n)\n)\n" ) );
		System.out.println( prettify( lispWords, "(\n(\n(\n(\n(\n)\n)\n)\n)\n)\n         )\n         )\n(\n(\n" ) );

	}
	
}
