package ats.pulsar.test;

import java.util.function.Function;

import kawa.standard.Scheme;

public class SchemeTest2 {
	public static Object exec( String script ) throws Throwable {
		return exec( script, null );
	}

	public static Object exec( String script, Function<Object,Object> proc ) throws Throwable {
		Scheme scheme = new Scheme();
		
		Object o = scheme.eval( script );
		if ( proc != null ) {
			o = proc.apply( o );
		}
		System.out.print( "script:" +  script + "=>" + o.toString() );
		System.out.print( "string:" +  o.toString() );
		System.out.print( "class :" +  o.getClass() );
		System.out.println();
		return o;
	}
	
	public static void main(String[] args) throws Throwable {
		exec( "0/5" );
		exec( "1/5" );
	}
}
