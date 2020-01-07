package pulsar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ArraySplitter<T> {
    List<T[]> output = new ArrayList<>();
    T[] input;
    T tokenBegin;
    T tokenEnd;
    public ArraySplitter(T[] input) {
        super();
        this.input = input;
    }
    public ArraySplitter(T[] input, T tokenBegin, T tokenEnd) {
        this.input = input;
        this.tokenBegin = tokenBegin;
        this.tokenEnd   = tokenEnd;
    }
    void add(int start,int end) {
        output.add( Arrays.copyOfRange( input, start , end ) );
    }
    
    void proc() {
        int level = 0;
        int start = 0;
        for ( int i=0; i<input.length; i++ ) {
            if ( tokenBegin.equals( input[i] ) ) {
                if ( level == 0 ) {
                    if ( start != i ) {
                        add( start, i );
                    }
                    start = i+1;
                }
                level=level+1;
            } else if ( tokenEnd.equals( input[i] ) ) {
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
    }
    public T[][] getResult() {
        return output.toArray((T[][])java.lang.reflect.Array.newInstance( this.input.getClass(), output.size() ) );
    }
    public List<T[]> getResultAsList() {
        return new ArrayList<>( output );
    }
    public static <T> T[][] splitBeginEnd(T[] a, T begin, T end ) {
        ArraySplitter<T> splitter = new ArraySplitter<T>( a, begin, end );
        splitter.proc();
        return splitter.getResult();
    }
    
    private static void testSplitter(String[] input ) {
        String[][] output = splitBeginEnd( input, "begin", "end" );
        for ( int i=0; i<output.length; i++ ) {
            String[] a = output[i];
            for ( int j=0; j<a.length; j++ ) {
                System.out.print( a[j] );
                System.out.print( "," );
            }
            System.out.println( );
        }
        System.out.println( "=========" );
    }
    
    public static void main(String[] args) {
        testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "bum", "begin","FOO","BAR", "end", } );
        testSplitter( new String[]{ "begin","foo","bar", "end", "bum", "begin","FOO","BAR", "end", } );
        testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "begin","FOO","BAR", "end", } );
        try {
            testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "FOO","BAR", "end", } );
            throw new Error( "missing exception" ) ;
        } catch ( RuntimeException e ) {
            System.err.println( e.getMessage() );
            System.err.println( "an exception is throw successfully. " );
        }
        try {
            testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "begin", "FOO","BAR",  } );
            throw new Error( "missing exception" ) ;
        } catch ( RuntimeException e ) { 
            System.err.println( e.getMessage() );
            System.err.println( "an exception is throw successfully." );
        }
        try {
            testSplitter( new String[]{ "hello", "begin","foo","bar", "end", "begin", "begin","FOO","end","aa","begin", "BAR", "end", "end"  } );
        } catch ( RuntimeException e ) { 
            System.err.println( e.getMessage() );
            System.err.println( "an exception is throw successfully." );
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