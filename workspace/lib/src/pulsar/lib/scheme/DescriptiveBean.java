package pulsar.lib.scheme;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import gnu.mapping.Procedure;

public abstract class DescriptiveBean implements DescriptiveBeanSynopsisFormatter, Cloneable {
	public static class Param implements Cloneable {
		List<String> names;
		String type;
		String defaultValue;
		boolean variable=false;
		String description;
		public Param(List<String> names, String type, String defaultValue, boolean variable, String description) {
			super();
			this.names = names;
			this.type = type;
			this.defaultValue = defaultValue;
			this.variable = variable;
			this.description = description;
		}
		public String getName() {
			if ( getNames().isEmpty() )
				throw new IllegalArgumentException();
			return getNames().get( 0 );
		}

		public List<String> getNames() {
			return names;
		}
		public void setNames(List<String> names) {
			this.names = names;
		}
		public String getType() {
			return type;
		}
		public void setType(String type) {
			this.type = type;
		}
		public String getDefaultValue() {
			return defaultValue;
		}
		public void setDefaultValue(String defaultValue) {
			this.defaultValue = defaultValue;
		}
		public boolean isVariable() {
			return variable;
		}
		public void setVariable(boolean variable) {
			this.variable = variable;
		}
		public String getDescription() {
			return description;
		}
		public void setDescription(String description) {
			this.description = description;
		}
		public Param process(Object[] args) {
			Param out = new Param(
				this.getNames(),
				this.getType(),
				this.getDefaultValue(),
				this.isVariable(),
				String.format( 
					this.getDescription(), args ));
			return out;
		}
		@Override
		public Param clone() {
			// TODO Auto-generated method stub
			try {
				Param result = (Param) super.clone();
				result.names = new ArrayList<>( this.names );
				return result;
			} catch (CloneNotSupportedException e) {
				throw new InternalError( e );
			}
		}
	}
	
	public DescriptiveBean() {
	}
	
	public DescriptiveBean(
			List<String> names, 
			String parameterDescription, String returnValueDescription,
	        String shortDescription, String longDescription) 
	{
		super();
		if ( names == null || names.isEmpty() )
			throw new IllegalArgumentException();
		this.names = names;
		
		this.parameterDescription = parameterDescription;
		this.returnValueDescription = returnValueDescription;
		this.shortDescription = shortDescription;
		this.longDescription = longDescription;
	}
	
	@Override
	public DescriptiveBean clone() {
		try {
			/*
			 * At this point, it is unknown that what class this object is going to be;
			 * though, Object#clone() supports duplicating unknown class.
			 */
			DescriptiveBean result = (DescriptiveBean)super.clone();
			result.names = this.names == null ? null : new ArrayList<>( this.names );
			result.parameterList = new ArrayList<>();
			for ( Param p : this.parameterList ) {
				result.parameterList.add( p.clone() );
			}
			return result;
		} catch (CloneNotSupportedException e) {
			throw new InternalError(e);
		}
	}
	
	private List<String> names;
	private String parameterDescription;
	private List<Param> parameterList = new ArrayList<>();
	private String returnValueDescription;
	private String shortDescription;
	private String longDescription;

	public String getName() {
		if ( getNames() == null || getNames().isEmpty() )
			throw new IllegalArgumentException();
		return getNames().get( 0 );
	}

	public List<String> getNames() {
		return names;
	}
	public DescriptiveBean setNames(String ... names) {
		this.setNames( Arrays.asList( names ) );
		return this;
	}
	public DescriptiveBean setNames(List<String> names) {
		if ( names.isEmpty() )
			throw new IllegalArgumentException();
		this.names = names;
		return this;
	}

	public abstract String formatParameter( Param param );

	public abstract String formatParameterDescription();

	public String getParameterDescription() {
		return parameterDescription;
	}
	public void setParameterDescription(String parameterDescription) {
		this.parameterDescription = parameterDescription;
	}
	public void addParameter( Param param ){
		this.parameterList.add( param );
	}
	public void addParameter( String name, String type, String defaultValue, boolean isVariable, String description ){
		this.addParameter( new Param( Arrays.asList( name ), type, defaultValue, isVariable, description ) );
	}
	public void addParameter( List<String> names, String type, String defaultValue, boolean isVariable, String description ){
		this.addParameter( new Param( names, type, defaultValue, isVariable, description ) );
	}
	public List<Param> getParameterList() {
		return parameterList;
	}
	public String getReturnValueDescription() {
		return returnValueDescription;
	}
	public void setReturnValueDescription(String returnValueDescription) {
		this.returnValueDescription = returnValueDescription;
	}
	public String getShortDescription() {
		return shortDescription;
	}
	public void setShortDescription(String shortDescription) {
		this.shortDescription = shortDescription;
	}
	public String getLongDescription() {
		return longDescription;
	}
	public void setLongDescription(String longDescription) {
		this.longDescription = longDescription;
	}

	public DescriptiveBean process( Object ... arguments ) {
		return process( this, arguments );
	}
	
	public String format() {
		return KawaPadDescriptive.formatForKawaPad( this );
	};
	
//	@Override
//	public abstract String formatSynopsis();

	static DescriptiveBean process( DescriptiveBean in, Object ... args ) {
		// See comment in DescriptiveBean#clone() 
		DescriptiveBean out = in.clone();
		
		out.setParameterDescription( String.format( in.getParameterDescription(), args ));
		for ( Param in_param : in.getParameterList() ) {
			out.addParameter( in_param.process( args ) );
		}
		
		out.setReturnValueDescription( String.format( in.getReturnValueDescription(), args )) ;
		out.setShortDescription( String.format ( in.getShortDescription(), args ));
		out.setLongDescription( String.format( in.getLongDescription(), args ));
		
		return out;
	}

	public static String formatForMarkdown( Procedure p ) {
		return formatForMarkdown( SchemeUtils.getDescriptionBean( p ) );
	}

	static Pattern p1 = Pattern.compile( "([\\(\\)\\[\\]])" );
	public static String formatForMarkdown( DescriptiveBean bean ) {
		StringBuilder message = new StringBuilder();
		
		String name = bean.getName().toUpperCase();

//		message.append( "========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========\n\n" );
		message.append( "" + name + "\n" );
		message.append( "====================" );
		message.append( "\n\n" );
		
		{
			message.append( 
					"#### SYNOPSIS ####\n" );
			message.append( "    " );
			message.append( bean.formatSynopsis());
			message.append( "\n\n" );
		}

		String msg1;
		{
			msg1 = bean.getShortDescription();
			msg1 = bean.interporlate( msg1 );
		}

		String msg2;
		{
			msg2 = bean.getLongDescription();
			msg2 = bean.interporlate( msg2 );
		}
		
		message.append( "### DESCRIPTION ###\n" );
		message.append( SchemeUtils.wrapMultiLine( msg1 + " " + msg2 , 80 ) );
		message.append( "\n\n" );
		message.append( "--------------------------------------------------------" );
		message.append( "" );

		return message.toString();	
	}


	String interporlate( String msg ) {
		return msg.replaceAll( "<name/>",  this.getName() );
	}
	



}
