package ats.pulsar.test;

import java.util.function.Function;

import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.SimpleSymbol;
import gnu.math.IntNum;
import kawa.standard.Scheme;

public class SchemeTest3 {
	public static Object exec( String script ) throws Throwable {
		return exec( script, null );
	}

	public static Object exec( String script, Function<Object,Object> proc ) throws Throwable {
		Scheme scheme = new Scheme();
		scheme.eval( "(require <ats.pulsar.SchemePulsar>)" );
		
		
		scheme.getEnvironment().define( SimpleSymbol.make( "", "helloo" ), null, new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				int sum=0;
				for ( int i=0; i<args.length; i++ ) {
					if ( args[i] instanceof IntNum  ) {
						sum += IntNum.intValue( args[i] );
					}
				}
				return sum;
			}
		});

		scheme.getEnvironment().define( SimpleSymbol.make( "", "reflect" ), null, new ProcedureN() {
			@Override
			public Object applyN(Object[] args) throws Throwable {
				@SuppressWarnings("unused")
				int sum=0;
				for ( int i=0; i<args.length; i++ ) {
					System.err.println(  args[i].getClass() );
					if ( args[i] instanceof Procedure ) {
						((Procedure)args[i]).applyN( new Object[] { 1 } );
					}
				}
				return 0;
			}
		});

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
		exec( "(+ 1 (helloo 1 2 3) )" );
		exec( "(reflect (lambda ( s )  (display 'hello-reflection )(newline)))" );
	}
}
