package ats.pulsar.test;

import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.SimpleSymbol;
import kawa.standard.Scheme;

public class SchemeTest4 {

	
	private static Object f0;
	public static void main(String[] args) throws Throwable {
		Scheme scheme = new Scheme();
		
		Thread t = new Thread( new Runnable() {
			@Override
			public void run() {
				try {
					f0= scheme.eval( 
							  "(define c 'global)" 
							+ "(define d 'Global)"
							+ "(let ((a 'hello)(b 'world))"
							+ "(lambda() a"
							+ ")"
							+ ")" );
					System.out.println( f0 );
					Thread.sleep(1000);
				} catch (Throwable e) {
					e.printStackTrace();
				}
			}
		});
		t.start();
		t.join();
		System.out.print( "START" );

		scheme.getEnvironment().define( SimpleSymbol.make( "", "f0" ), null, f0 );

		{
			String script = "(f0)";
			Object o = scheme.eval( script );
			display(script, o);
		}

		{
			Procedure p0 = (Procedure) f0;
			display("", p0.apply0() );
		}

		Environment.setCurrent(null);
		{
			Procedure p0 = (Procedure) f0;
			display("", p0.apply0() );

		}
		{
			Procedure p0 = (Procedure) f0;
			display("", p0.applyN(new Object[] {}) );
		}
		

	}


	public static void display(String script, Object o) {
		System.out.print( "script:" +  script + "=>" + o.toString() );
		System.out.print( "string:" +  o.toString() );
		System.out.print( "class :" +  o.getClass() );
		System.out.println();
	}
}
