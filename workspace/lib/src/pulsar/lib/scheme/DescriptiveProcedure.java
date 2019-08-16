package pulsar.lib.scheme;

import gnu.mapping.Procedure;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;

public interface DescriptiveProcedure {
	default Procedure getProcedure() {
		return (Procedure)this;
	}
	SimpleSymbol LONG_DESCRIPTION = Symbol.valueOf( "pulsar-long-description" );
	SimpleSymbol SHORT_DESCRIPTION = Symbol.valueOf( "pulsar-short-description" );
	default String getLongDescription() {
		return getProcedure().getProperty( LONG_DESCRIPTION, "" ).toString();
	}
	default String getShortDescription() {
		return getProcedure().getProperty( SHORT_DESCRIPTION, "" ).toString();
	}
	default void setLongDescription( String description ) {
		getProcedure().setProperty( LONG_DESCRIPTION, SchemeUtils.toSchemeString( description ) );
	}
	default void setShortDescription( String description ) {
		getProcedure().setProperty( SHORT_DESCRIPTION, SchemeUtils.toSchemeString( description ) );
	}
}
