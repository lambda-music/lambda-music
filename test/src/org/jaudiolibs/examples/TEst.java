package org.jaudiolibs.examples;

import java.util.ArrayDeque;

public class TEst {
	public static void main(String[] args) {
		Object o = new Object();
		System.out.println( o.equals( o ));
		
		
		ArrayDeque<String> d = new ArrayDeque<String>();
		
		for ( int i=0; i<10; i++ ) {
			d.offer( Integer.toString(i));
		}
		
		System.out.print( "" );
		for ( String s : d ) {
			System.out.print( s );
		}
		System.out.print( "" );
		
		while ( ! d.isEmpty() ) {
			System.out.print( d.poll() );
		}
	}
}
