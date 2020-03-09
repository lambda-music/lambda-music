package lamu;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * This class parses the given elements as nested blocks which are surrounded 
 * by the specified begin end tag elements, and returns a list of the parsed blocks.
 * 
 * <h3>Examples</h3>
 * <pre>
 * // The elements not surrounded by begin/end will return as they are.
 * [ "hello" , "world" ]
 *        => [ [ "hello", "world" ] ]
 *        
 * // The elements which are surrounded by a set of begin/end will be parsed as an element list.
 * [ "begin", "hello" , "world" , "end" ]
 *        => [ [ "hello", "world" ] ]
 *        
 * // You can specify multiple spans. 
 * [ "begin", "hello" , "world" , "end", "begin", "HELLO" , "WORLD" , "end"]
 *        => [ [ "hello", "world" ],  [ "HELLO", "WORLD" ], ]
 * 
 * // The elements which are not surrounded by begin/end tag elements are also treated as a block. 
 * [ "FOO", "begin", "hello" , "world" , "end", "MIDDLE", "begin", "HELLO" , "WORLD" , "end", "BAR" ]
 *        => [[ "FOO" ], [ "hello", "world" ], ["MIDDLE"], [ "HELLO", "WORLD" ], ["BAR"] ]
 *        
 * // A block contains nested blocks is parsed as the outermost block.
 * [ "begin", "foo", "begin", "bar", "end", "bum", "end" ]
 *        => [ [ "foo", "begin", "bar", "end", "bum" ] ]
 *        
 * // Any unmatched begin/end tag elements causes an exception to be thrown.
 * [ "begin", "foo", "begin", "bar", "**FALSE END **", "bum", "end" ]
 *        => an exception to be thrown
 * </pre>
 * 
 *  See {@link lamu.LamuApplicationArraySplitter#splitBeginEnd }.
 */
public class LamuApplicationArraySplitter<T> {
	/**
	 * Executes the process.
	 * 
	 * @param list
	 *            Specifies the list to be parsed.
	 * @param begin
	 *            Specifies the tag to begin a block.
	 * @param end
	 *            Specifies the tag to end the block.
	 * @return Returns a list of lists that contains blocks.
	 */
    public static <T> List<List<T>> splitBeginEnd(List<T> list, T begin, T end ) {
        return new LamuApplicationArraySplitter<T>( list, begin, end ).execute();
    }
    
    boolean executed = false;
	List<T> input;
    List<List<T>> output = new ArrayList<>();
    T tokenBegin;
    T tokenEnd;
    public LamuApplicationArraySplitter( List<T> input, T tokenBegin, T tokenEnd ) {
        this.input      = input;
        this.tokenBegin = tokenBegin;
        this.tokenEnd   = tokenEnd;
    }
    private void add( int start,int end ) {
    	this.output.add( new ArrayList<T>( input.subList( start, end ) ) );
    }
    private void addEmpty() {
    	this.output.add( new ArrayList<T>() );
    }
    private void proc() {
    	this.executed = true;
    	
    	if ( this.input.isEmpty() ) {
    		this.addEmpty();
    		return;
    	}
        int level = 0;
        int start = 0;
        for ( int i=0; i<input.size() ; i++ ) {
            if ( tokenBegin.equals( input.get(i) ) ) {
                if ( level == 0 ) {
                    if ( start != i ) {
                        add( start, i );
                    }
                    start = i+1;
                }
                level=level+1;
            } else if ( tokenEnd.equals( input.get(i) ) ) {
                if ( level == 0 ) {
                    throw new RuntimeException( "end without begin error." );
                }
                level=level-1;
                if ( level == 0 ) {
                    add( start, i );
                    start = i + 1;
                }
            }
        }
        if ( level != 0 ) {
            throw new RuntimeException( "missing end error." );
        }
        if ( start != input.size() ) {
        	this.add( start, input.size() );
        }
    }
    public List<List<T>> getResult() {
        return new ArrayList<>( output );
    }
    public List<List<T>> execute() {
    	this.proc();
    	return getResult();
    }
    
    /**
     * This class performs the simple test for {@link LamuApplicationArraySplitter}.
     */
    static class Test {
        static int counter = 0; 
        private static void testSplitter( String[] input ) {
        	counter ++;
        	System.out.println( "==== Test[" + counter + "] ====" );
        	List<List<String>> output = splitBeginEnd( Arrays.asList(input), "begin", "end" );
            for ( int i=0; i<output.size(); i++ ) {
                List<String> a = output.get(i);
                System.out.print( "[" + i + "]:" + String.join(",", a) );
                System.out.println();
            }
        }
        
        public static void main(String[] args) {
            testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "bum", "begin","FOO","BAR", "end", } );
            testSplitter( new String[]{ "begin","foo","bar", "end", "bum", "begin","FOO","BAR", "end", } );
            testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "begin","FOO","BAR", "end", } );
            testSplitter( new String[]{ "hello", "world" } );
            testSplitter( new String[]{} );
            testSplitter( new String[]{ "hello", "begin","foo","begin", "bar", "end", "bum", "end", "world" } );
            try {
                testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "FOO","BAR", "end", } );
                throw new Error( "missing exception" ) ;
            } catch ( RuntimeException e ) {
            	errorMessage( e, "an exception is throw successfully" );
            }
            try {
                testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "begin", "FOO","BAR",  } );
                throw new Error( "missing exception" ) ;
            } catch ( RuntimeException e ) { 
            	errorMessage( e, "an exception is throw successfully" );
            }
            try {
                testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "begin", "begin","FOO","end","aa","begin", "BAR", "end", "end"  } );
            } catch ( RuntimeException e ) { 
            	errorMessage( e, "an exception is throw successfully" );
            }
        }
    	private static void errorMessage(RuntimeException e, String msg) {
    		System.out.println( "### " + e.getMessage() + " ###" );
    		System.out.println( "### " +msg + " ###" );
    	}
    }
    
    
    static <T> int indexOf( T[] a, T v, int from ) {
        for ( int i=from; i<a.length; i++ ) {
            if ( v == a[i] || v.equals( a[i] ) )
                return i;
        }
        return -1;
    }
    static <T> T[][] splitArray(T[] a, T separator ) {
        ArrayList<T[]> result = new ArrayList<>();
        int last = 0;
        for(;;){
            int i = indexOf( a, separator, last );
            if ( i < 0 ) {
                result.add( Arrays.copyOfRange( a, last , a.length ) );
                break;
            }
            if ( last != i ) 
                result.add( Arrays.copyOfRange( a, last , i ) );
            last = i+1;
        }
        if ( result.size() == 0 ) {
            result.add( a.clone() );
        }
        return result.toArray((T[][])java.lang.reflect.Array.newInstance( a.getClass(), result.size() ) );
    }
}