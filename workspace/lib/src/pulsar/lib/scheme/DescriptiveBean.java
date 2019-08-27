package pulsar.lib.scheme;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import gnu.mapping.Procedure;

public class DescriptiveBean {
	public static class Param {
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
		public String joinNames() {
//			if ( getNames().isEmpty() )
//				throw new IllegalArgumentException();
//			return getNames().get( 0 );
			return String.join( "|" , this.getNames() );
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
		@Override
		public String toString() {
			String result = joinNames() + "::" + getType();
			if ( defaultValue != null ) {
				result =  "[" + result + "=" + defaultValue +  "]";
				if ( this.variable ) {
					result =  result + "...";
				}
			} else {
				if ( this.variable ) {
					result =  "[" + result + "]...";
				}
			}
			return result;
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

	/* (non-Javadoc)
	 * @see pulsar.lib.scheme.Descriptive#getParameterDescription2()
	 */
	public String getParameterDescription2() {
		List<String> stringList = new ArrayList<>();
		for ( Param param : this.parameterList ) {
			stringList.add( param.toString() );
		}
		return String.join( " ", stringList );
	}

	/* (non-Javadoc)
	 * @see pulsar.lib.scheme.Descriptive#getParameterDescription()
	 */
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
	/* (non-Javadoc)
	 * @see pulsar.lib.scheme.Descriptive#getParameterList()
	 */
	public List<Param> getParameterList() {
		return parameterList;
	}
	/* (non-Javadoc)
	 * @see pulsar.lib.scheme.Descriptive#getReturnValueDescription()
	 */
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
	/* (non-Javadoc)
	 * @see pulsar.lib.scheme.Descriptive#getLongDescription()
	 */
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
		return formatForKawaPad( this );
	}

	static DescriptiveBean process( DescriptiveBean in, Object ... args ) {
		DescriptiveBean out = new DescriptiveBean();
		
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
			String syn = bean.getParameterDescription2();
			String rv = bean.getReturnValueDescription();
			
			message.append( 
					"#### SYNOPSIS ####\n" );
			message.append( "    (" +
					String.join( "|", bean.getNames()) +
					(syn.equals("") ? "" : " ") +
					syn +
					")" + rv);
			
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
	
	public static String formatForKawaPad( DescriptiveBean bean ) {
		StringBuilder message = new StringBuilder();
		
		message.append( "========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========\n\n" );
		message.append( "NAME: " );
		message.append( bean.getName().toUpperCase()  );
		message.append( "\n\n" );
		{
			String syn = bean.getParameterDescription2();
			String rv = bean.getReturnValueDescription();
			message.append( 
					"SYNOPSIS: (" +
					String.join( "|", 
						bean.getNames()) +
					(syn.equals("") ? "" : " ") +
					syn +
					")" + rv);
			
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
		
		
		message.append( "DESCRIPTION: " );
		message.append(  msg1  );
		message.append( " " );
		message.append(  msg2  );
		message.append( "\n\n" );
		message.append( "======================================================================" );
		message.append( "" );

		return message.toString();
	}

}
