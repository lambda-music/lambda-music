package ats.pulsar.editor;

public class SimpleSchemeIndentChanger {
	/**
	 * Returns the next index of the nearest \n on the left side of the specified
	 * position. Note that the \n itself considered that it belongs to the the
	 * previous line not following line.
	 * 
	 * ex) s =  "0123456\n" +
	 *          "89abcde\n" +
	 *          "hello";
	 *          
	 *     lookupLineStart( s, 9 ) => 8
	 *     lookupLineStart( s, 8 ) => 8
	 *     lookupLineStart( s, 7 ) => 0
	 */
	public static int lookupLineStart( String s, int pos ) {
		for ( int i=pos-1; ;i-- ) {
			if ( i<0 ) 
				return 0;
			
			if ( s.charAt(i) == '\n' ) {
				return i+1;
			}
		}
	}

	/**
	 * Returns the next index of nearest \n on the right side of the specified 
	 * position. Note that this function regards the first position of the line belongs
	 * to the previous line.
	 * ex) s =  "0123456\n" +
	 *          "89abcde\n" +
	 *          "hello";
	 *          
	 *     lookupLineEnd( s, 7 ) =>  8
	 *     lookupLineEnd( s, 8 ) =>  8
	 *     lookupLineEnd( s, 9 ) => 16

	 * @param s
	 *   a string object to lookup.
	 * @param pos
	 *   the position where to start lookup.
	 * @return
	 *   the position that we want to know.
	 */
	public static int lookupLineEnd( String s, int pos ) {
		for ( int i=pos-1; ;i++ ) {
			if ( s.length() <= i ) 
				return s.length();
			
			if ( s.charAt(i) == '\n' ) {
				return i+1;
			}
		}
	}

	public static String fillStr ( char c, int length ) {
		StringBuffer sb = new StringBuffer();
		for ( int i=0; i<length; i++ )
			sb.append(c);
		return sb.toString();
	}

//	public static String incIndent( String s, String indent ) {
//		String[] strings = s.split("\n" );
//		for ( int i=0; i<strings.length; i++ ) {
//			strings[i] = indent + strings[i];
//		}
//		return String.join( "\n", strings );
//	}
	
//	public static String incIndent( String s, int indent ) {
//		return incIndent( s, fillStr( ' ', indent ) );
//	}
	
	static int countFirstSpaces( String s ) {
		for ( int i=0; i<s.length(); i++ ) {
			if ( s.charAt(i) != ' ' ) {
				return i;
			}
		}
		// This means that the string contains only spaces.
		return s.length();
	}
	public static String changeIndentRelativeMultiline( String s, int indent ) {
		String[] strings = s.split( "\n" );
		for ( int i=0; i<strings.length; i++ ) {
			int count = countFirstSpaces( strings[i] );
			strings[i] = fillStr( ' ',  count + indent ) + strings[i].substring(count);
		}
		
		return String.join( "\n", strings );
	}
	public static String changeIndentAbsoluteMultiline( String s, int indent ) {
		String[] strings = s.split( "\n" );
		for ( int i=0; i<strings.length; i++ )
			strings[i] = changeIndentAbsolute( strings[i], indent );
		return String.join( "\n", strings );
	}
	public static String changeIndentAbsolute( String s, int indent ) {
		return fillStr( ' ',  indent ) + s.substring(countFirstSpaces( s ));
	}
	
	public static void main(String[] args) {
		String sample = "            hello\n        world\n    foo\n";
		System.out.println( lookupLineStart( sample, 12 ) );
		System.out.println(  changeIndentRelativeMultiline( sample , 16 ) );
		System.out.println(  changeIndentRelativeMultiline( sample ,  8 ) );
		System.out.println(  changeIndentRelativeMultiline( sample ,  0 ) );
		System.out.println(  changeIndentRelativeMultiline( sample , -4 ) );
		System.out.println(  changeIndentRelativeMultiline( sample , -8 ) );
		System.out.println(  changeIndentRelativeMultiline( sample ,-16 ) );
	}
}
