package nu.oka.metro;

import java.util.ArrayList;

public class Test {
	public static void main(String[] args) {
		Object[] arr = new Object[] {
				new Object() { public String toString() { return "01"; } },
				new Object() { public String toString() { return "02"; } },
				new Object() { public String toString() { return "03"; } },
				new Object() { public String toString() { return "04"; } },
				new Object() { public String toString() { return "05"; } },
		};
		ArrayList<Object> lst1 = new ArrayList<Object>( );
		lst1.add( arr[0] );
		lst1.add( arr[1] );
		lst1.add( arr[2] );
		lst1.add( arr[3] );
		lst1.add( arr[4] );

		ArrayList<Object> lst2 = new ArrayList<Object>( );
		lst2.add( arr[0] );
		lst2.add( arr[1] );

		lst1.removeAll( lst2 );
		
		System.out.println(lst1 );
		
	}
}
