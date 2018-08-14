package ats.metro;

import java.util.ArrayDeque;

public class TEst {
	public static void main(String[] args) {
		Object o = new Object();
		System.out.println( o.equals( o ));
		
		
		ArrayDeque<String> d = new ArrayDeque<String>();
		
		for ( int i=0; i<10; i++ ) {
			d.offer( Integer.toString(i));
		}
		
		System.out.println("");
		for ( String s : d ) {
			System.out.println( s );
		}
		System.out.println("");
		
		while ( ! d.isEmpty() ) {
			System.out.println( d.poll() );
		}
	}
}
