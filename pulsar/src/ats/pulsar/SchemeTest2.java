package ats.pulsar;

import java.util.function.Function;

import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.SimpleSymbol;
import gnu.math.IntNum;
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
		System.out.println( "script:" +  script + "=>" + o.toString() );
		System.out.println( "string:" +  o.toString() );
		System.out.println( "class :" +  o.getClass() );
		System.out.println();
		return o;
	}
	
	public static void main(String[] args) throws Throwable {
		exec( "0/5" );
		exec( "1/5" );
	}
}
