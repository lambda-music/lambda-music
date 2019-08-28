package pulsar;

import java.util.Arrays;
import java.util.List;

import kawa.standard.Scheme;
import pulsar.lib.scheme.DescriptiveBean;
import pulsar.lib.scheme.DescriptiveDocumentType;

public class PulsarDocuments {
	private PulsarDocuments() {}
	public static void defineDoc(Scheme scheme, NoteListParser parser ) {
		List<NoteListParserElement> allElements = parser.getAllElements();
//		Collections.reverse( allElements );
		
		for ( NoteListParserElement element : allElements ) {
			defineDoc( scheme, element );
		}
	}
	public static String nullCheck( String s ) {
		if ( s == null )
			return "";
		else 
			return s;
	}
	public static void defineDoc(Scheme scheme, NoteListParserElement element ) {
		DescriptiveBean bean = new PulsarNoteListDescriptiveBean(); 
		if ( element.getLongName() == null ) {
			if ( element.getShortName() ==null ) {
				bean.setNames( "no-name" );
			} else {
				bean.setNames( element.getShortName() );
			}
		} else {
			if ( element.getShortName() ==null ) {
				bean.setNames( element.getLongName() );
			} else {
				bean.setNames(  element.getLongName(), element.getShortName() );
			}
		}
		bean.setParameterDescription( "" );
		for ( NoteListParserElementParameter p : element.getParameters() ) {
			List<String> list;
			if ( p.getShortName() == null ) {
				if ( p.getLongName() == null ) {
					list = Arrays.asList( "no-name" );
				} else {
					list = Arrays.asList( p.getShortName() );
				}
			} else {
				if ( p.getLongName() == null ) {
					list = Arrays.asList( p.getShortName() );
				} else {
					list = Arrays.asList( p.getLongName(), p.getShortName() );
				}
			}
			bean.addParameter(
				list,
				nullCheck( p.getType() ), 
				nullCheck( p.getDefaultValue()), 
				false, 
				nullCheck( p.getDescription() ) );
		}
		bean.setReturnValueDescription( "" );
		bean.setShortDescription( nullCheck( element.getShortDescription() ) );
		bean.setLongDescription( nullCheck( element.getLongDescription() ) );
		
		DescriptiveDocumentType.defineNoteDoc( scheme, bean );
	}
}
