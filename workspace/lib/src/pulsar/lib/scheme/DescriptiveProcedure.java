package pulsar.lib.scheme;

import java.util.List;

import gnu.mapping.Procedure;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;

public interface DescriptiveProcedure {
	default Procedure getProcedure() {
		return (Procedure)this;
	}
	SimpleSymbol LONG_DESCRIPTION  = Symbol.valueOf( "pulsar-long-description" );
	SimpleSymbol SHORT_DESCRIPTION = Symbol.valueOf( "pulsar-short-description" );
	SimpleSymbol PARAMETER_DESCRIPTION  = Symbol.valueOf( "pulsar-parameter-description" );
	SimpleSymbol RETURN_VALUE_DESCRIPTION  = Symbol.valueOf( "pulsar-return-value-description" );
	SimpleSymbol PROCEDURE_NAME_LIST = Symbol.valueOf( "pulsar-command-name-list" );
	default String getLongDescription() {
		return getProcedure().getProperty( LONG_DESCRIPTION, "" ).toString();
	}
	default String getShortDescription() {
		return getProcedure().getProperty( SHORT_DESCRIPTION, "" ).toString();
	}
	default String getParameterDescription() {
		return getProcedure().getProperty( PARAMETER_DESCRIPTION ,"" ).toString();
	}
	default String getReturnValueDescription() {
		return getProcedure().getProperty( RETURN_VALUE_DESCRIPTION ,"" ).toString();
	}
	default List<Symbol> getNameList() {
		return (List<Symbol>) getProcedure().getProperty( PROCEDURE_NAME_LIST ,"" );
	}
	
	default void setLongDescription( String description ) {
		getProcedure().setProperty( LONG_DESCRIPTION, SchemeUtils.toSchemeString( description ) );
	}
	default void setShortDescription( String description ) {
		getProcedure().setProperty( SHORT_DESCRIPTION, SchemeUtils.toSchemeString( description ) );
	}
	default void setParameterDescription( String description ) {
		getProcedure().setProperty( PARAMETER_DESCRIPTION, SchemeUtils.toSchemeString( description ) );
	}
	default void setReturnValueDescription( String description ) {
		getProcedure().setProperty( RETURN_VALUE_DESCRIPTION, SchemeUtils.toSchemeString( description ) );
	}
	default void setNameList( List<Symbol> list ) {
		getProcedure().setProperty( PROCEDURE_NAME_LIST, list );
	}
	
	SimpleSymbol PULSAR_INITIALIZER_A = Symbol.valueOf( "pulsar-initializer-a" );
	SimpleSymbol PULSAR_INITIALIZER_B = Symbol.valueOf( "pulsar-initializer-b" );

	default DescriptiveInitializerA getInitializerA() {
		return (DescriptiveInitializerA)getProcedure().getProperty( PULSAR_INITIALIZER_A, null );
	}
	default DescriptiveInitializerB getInitializerB() {
		return (DescriptiveInitializerB)getProcedure().getProperty( PULSAR_INITIALIZER_B, null );
	}
	default void setInitializerA(DescriptiveInitializerA initializerA) {
		getProcedure().setProperty( PULSAR_INITIALIZER_A, initializerA );
	}
	default void setInitializerB(DescriptiveInitializerB initializerB) {
		getProcedure().setProperty( PULSAR_INITIALIZER_B, initializerB );
	}

	default void init() {
		DescriptiveProcedure procedure = this;
		DescriptiveInitializerA a = getInitializerA();
		DescriptiveInitializerB b = getInitializerB();
		Object[] args = b.arguments.toArray();
		procedure.setParameterDescription( String.format( a.getParameterDescription(), args ));
		procedure.setReturnValueDescription( String.format( a.getReturnValueDescription(), args )) ;
		procedure.setShortDescription( String.format ( a.getShortDescription(), args ));
		procedure.setLongDescription( String.format( a.getLongDescription(), args ));
	}

}
