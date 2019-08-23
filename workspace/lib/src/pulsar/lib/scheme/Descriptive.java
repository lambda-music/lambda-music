package pulsar.lib.scheme;

import gnu.mapping.Procedure;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;

public interface Descriptive {
	SimpleSymbol DESCRIPTION  = Symbol.valueOf( "pulsar-description" );
	public static String getDescription( Object o ) {
		if ( o instanceof Procedure ) {
			Object value = ((Procedure)o).getProperty( DESCRIPTION, null );
			return value==null?null:value.toString();
		} else {
			SchemeUtils.logWarn( "Descriptive#getDescription(): WARNING:" + o + " is not procedure" );
			return null;
		}
	}
	public static void setDescription( Object o, String description ) {
		if ( o instanceof Procedure ) {
			((Procedure)o).setProperty( DESCRIPTION, SchemeUtils.toSchemeString( description ) );
		} else {
			SchemeUtils.logWarn( "Descriptive#setDescription(): WARNING:" + o + " is not procedure" );
		}
	}

	default String getDescription() {
		return getDescription( (Procedure)this );
	}
	default void setDescription( String description ) {
		setDescription( (Procedure)this, description );
	}
}
