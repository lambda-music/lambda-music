package pulsar.lib.scheme;

import java.util.Arrays;
import java.util.List;

import gnu.mapping.Symbol;

public class DescriptiveInitializerBean {
	public DescriptiveInitializerBean() {
	}
	
	public DescriptiveInitializerBean(
			String parameterDescription, String returnValueDescription,
	        String shortDescription, String longDescription, List<Symbol> nameList) 
	{
		super();
		this.parameterDescription = parameterDescription;
		this.returnValueDescription = returnValueDescription;
		this.shortDescription = shortDescription;
		this.longDescription = longDescription;
		this.nameList = nameList;
	}

	private String parameterDescription;
	private String returnValueDescription;
	private String shortDescription;
	private String longDescription;
	private List<Symbol> nameList;
	public List<Symbol> getNameList() {
		return nameList;
	}
	public void setNameList(List<Symbol> nameList) {
		this.nameList = nameList;
	}
	public void setNameList( String ... strings ) {
		this.nameList = Arrays.asList( SchemeUtils.stringListToSymbolList( strings ) );
	}
	public String getParameterDescription() {
		return parameterDescription;
	}
	public void setParameterDescription(String parameterDescription) {
		this.parameterDescription = parameterDescription;
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

	public DescriptiveInitializerBean process( Object ... arguments ) {
		return process( this, arguments );
	}
	
	public String format() {
		return formatForKawaPad( this );
	}

	static DescriptiveInitializerBean process( DescriptiveInitializerBean in, Object ... args ) {
		DescriptiveInitializerBean out = new DescriptiveInitializerBean();
		
		out.setParameterDescription( String.format( in.getParameterDescription(), args ));
		out.setReturnValueDescription( String.format( in.getReturnValueDescription(), args )) ;
		out.setShortDescription( String.format ( in.getShortDescription(), args ));
		out.setLongDescription( String.format( in.getLongDescription(), args ));
		
		return out;
	}
	
	static String formatForMarkdown( DescriptiveInitializerBean bean ) {
		StringBuilder message = new StringBuilder();
		List<String> names = SchemeUtils.symbolListToStringList( bean.getNameList());
		
		String name = names.isEmpty() ? "PULSAR" :  names.get(0).toUpperCase();

		message.append( "========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========\n\n" );
		message.append( "### " + name + " ### " );
		message.append( "\n\n" );
		{
			String syn = bean.getParameterDescription();
			String rv = bean.getReturnValueDescription();
			message.append( 
					"#### SYNOPSIS ####\n\n" );
			message.append( "(" +
					String.join( "|", 
						names) +
					(syn.equals("") ? "" : " ") +
					syn +
					")" + rv);
			
			message.append( "\n\n" );
		}

		String msg1;
		{
			msg1 = bean.getShortDescription();
			if ( "".equals( msg1 ) ) {
			} else {
				List<Symbol> l = bean.getNameList();
				if ( l != null && ! l.isEmpty() ) {
					msg1 = msg1.replaceAll( "<procedure-name/>",  SchemeUtils.symbolToString( l.get(0) ) );
					// msg1 = "||" + SchemeUtils.symbolToString( l.get(0) ) + "||" + " " + msg1;
				} else {
//					msg1 = "This " + msg1;
				}
			}
		}

		String msg2;
		{
			msg2 = bean.getLongDescription();
		}
		
		message.append( "### DESCRIPTION ###" );
		message.append(  msg1  );
		message.append( " " );
		message.append(  msg2  );
		message.append( "\n\n" );
		message.append( "--------------------------------------------------------" );
		message.append( "" );

		return message.toString();	
	}

	static String formatForKawaPad( DescriptiveInitializerBean bean ) {
		StringBuilder message = new StringBuilder();
		List<String> names = SchemeUtils.symbolListToStringList( bean.getNameList());
		
		message.append( "========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========\n\n" );
		message.append( "NAME: " );
		message.append( names.isEmpty() ? "PULSAR" :  names.get(0).toUpperCase()  );
		message.append( "\n\n" );
		{
			String syn = bean.getParameterDescription();
			String rv = bean.getReturnValueDescription();
			message.append( 
					"SYNOPSIS: (" +
					String.join( "|", 
						names) +
					(syn.equals("") ? "" : " ") +
					syn +
					")" + rv);
			
			message.append( "\n\n" );
		}

		String msg1;
		{
			msg1 = bean.getShortDescription();
			if ( "".equals( msg1 ) ) {
			} else {
				List<Symbol> l = bean.getNameList();
				if ( l != null && ! l.isEmpty() ) {
					msg1 = msg1.replaceAll( "<procedure-name/>",  SchemeUtils.symbolToString( l.get(0) ) );
					// msg1 = "||" + SchemeUtils.symbolToString( l.get(0) ) + "||" + " " + msg1;
				} else {
//					msg1 = "This " + msg1;
				}
			}
		}

		String msg2;
		{
			msg2 = bean.getLongDescription();
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
