package pulsar;

import java.util.Arrays;
import java.util.List;

import kawa.standard.Scheme;
import quartz.lib.scheme.doc.DescriptiveBean;
import quartz.lib.scheme.doc.DescriptiveDocumentCategory;

public class PulsarDocuments {
    public static final DescriptiveDocumentCategory DOCS = 
            DescriptiveDocumentCategory.createCategory( "pulsar-procedures" );

    public static final DescriptiveDocumentCategory NOTES = 
            DescriptiveDocumentCategory.createCategory( "pulsar-notations" );

    private PulsarDocuments() {}
    public static void defineDoc(Scheme scheme, NoteListParser parser ) {
        List<NoteListParserElement> allElements = parser.getAllElements();
//      Collections.reverse( allElements );
        
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
    public static final String PREFIX = "note-event-"; 
    public static void defineDoc(Scheme scheme, NoteListParserElement element ) {
        DescriptiveBean bean = new PulsarNoteListDescriptiveBean(); 
        if ( element.getLongName() == null ) {
            if ( element.getShortName() ==null ) {
                bean.setNames( "no-name" );
            } else {
                bean.setNames( element.getShortName() );
            }
        } else {
            if ( element.getShortName() == null ) {
                bean.setNames( element.getLongName() );
            } else {
                bean.setNames(  element.getLongName(), element.getShortName() );
            }
        }
        bean.setParameterDescription( "" );
        for ( NoteListParserElementParameter p : element.getParameters() ) {
            List<String> nameList;
            if ( p.getShortName() == null ) {
                if ( p.getLongName() == null ) {
                    nameList = Arrays.asList( "no-name" );
                } else {
                    nameList = Arrays.asList( p.getShortName() );
                }
            } else {
                if ( p.getLongName() == null ) {
                    nameList = Arrays.asList( p.getShortName() );
                } else {
                    nameList = Arrays.asList( p.getLongName(), p.getShortName() );
                }
            }
            bean.addParameter( 0,
                nameList,
                nullCheck( p.getType() ), 
                nullCheck( p.getDefaultValue()), 
                false, 
                nullCheck( p.getDescription() ) );
        }
        bean.setReturnValueDescription( "" );
        bean.setShortDescription( nullCheck( element.getShortDescription() ) );
        bean.setLongDescription( nullCheck( element.getLongDescription() ) );
        
        NOTES.defineDoc( scheme.getEnvironment(), "note-" + bean.getName(),  bean );
    }
}
