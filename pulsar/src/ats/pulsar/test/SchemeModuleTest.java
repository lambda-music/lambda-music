package ats.pulsar.test;

import gnu.mapping.ProcedureN;
import gnu.math.IntNum;

class MyFunc extends ProcedureN
{
	// An "argument" that is part of each procedure instance.
	private Object arg0;

	public MyFunc(String name, Object arg0)
	{
		super( name );
		this.arg0 = arg0;
	}

	@Override
	public Object apply1 (Object arg1)
	{
		// Here you can so whatever you want. In this example,
		// we return a pair of the argument and arg0.
		return gnu.lists.Pair.make(arg0, arg1);
	}
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
}

public class SchemeModuleTest {
	public static final String foooo = "HELLO";
	public static final Object hello = new MyFunc( "hello", IntNum.make(1000));
}
