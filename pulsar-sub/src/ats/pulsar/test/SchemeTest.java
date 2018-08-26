package ats.pulsar.test;

import java.util.Map;
import java.util.function.Function;

import ats.pulsar.SchemeUtils;
import gnu.expr.CompiledProc;
import gnu.lists.Pair;
import gnu.mapping.SimpleSymbol;
import gnu.math.IntNum;
import kawa.standard.Scheme;

public class SchemeTest {
	public static Object exec( String script ) throws Throwable {
		return exec( script, null );
	}

	public static Object exec( String script, Function<Object,Object> proc ) throws Throwable {
		Scheme scheme = new Scheme();
		scheme.eval( "(require <org.jaudiolibs.test.SchemePulsar>)" );
		scheme.getEnvironment().define( SimpleSymbol.make( "", "helloo" ),null, IntNum.make(5) );
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
		exec( "(display 'hello)(newline)" );
		exec( "'()" );
		exec( "'1" );
		exec( "(list 1 2 3)");
		exec( "(list 1 2 3)", (o)->{
			Pair p = (Pair)o;
			return p.get(2);
		});
		exec( "(list 1 2 3)", (o)->{
			Pair p = (Pair)o;
			return p.getCdr();
		});
		exec( "'hello" );
		exec( "'hello-world" );
		exec( "\"hello world\"" );
		exec( "#t" );
		exec( "#f" );
		exec( "helloo" );
		exec( "(lambda () (display 'hello)(newline))" );
		exec( "(lambda () (display 'hello)(newline)'hello)", (o)->{
			try {
				return ((CompiledProc)o).applyN( new Object[] {} );
			} catch (Throwable e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return null;
			}
		});

		exec( "(hello 1 2 3)" );
		
		exec( "'((hello . HELLO) 1 (world . WORLD ) 2 3)", (o)->{
			Map<String,Object> map = SchemeUtils.list2map( (Pair)o, (type)->{
				return (Integer i )-> { 
					return "arg" + Integer.toString(i);
				};
			});
			// System.out.println( map );
			return map;
		});

//		// exec( "(hello 1 2 3 )");
//		exec( "'((hello 1)(foo 2))", (o)->{
//			return ((Pair)o).
//		});
	}
}
