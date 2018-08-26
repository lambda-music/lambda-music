package ats.pulsar.test;

import kawa.standard.Scheme;

public class SchemeTest04 {
	public static void main(String[] args) throws Throwable {
		Scheme scheme = new Scheme();
		System.out.println( scheme.eval( "#t" ).getClass() );
		System.out.println( scheme.eval( "#f" ).getClass() );
		System.out.println( scheme.eval( "'#t" ).getClass() );
		System.out.println( scheme.eval( "'#f" ).getClass() );
		System.out.println( scheme.eval( "'#true" ).getClass() );
		System.out.println( scheme.eval( "'#false" ).getClass() );
		System.out.println( scheme.eval( "#true" ).getClass() );
		System.out.println( scheme.eval( "#false" ).getClass() );
		System.out.println( scheme.eval( "'true" ).getClass() );
		System.out.println( scheme.eval( "'falses" ).getClass() );

	}
}
