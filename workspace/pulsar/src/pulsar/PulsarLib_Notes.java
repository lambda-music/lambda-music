package pulsar;

import java.util.Arrays;
import java.util.List;

import gnu.mapping.Environment;
import lamu.lib.doc.LamuDocument;
import lamu.lib.doc.NoteListSynopsisFormatter;
import lamu.lib.scheme.SchemeUtils;

public class PulsarLib_Notes {
    public static final String NOTES_ID = "pulsar-notations";

    private PulsarLib_Notes() {}
    public static void defineDoc( Environment env, NoteListParser parser ) {
        List<NoteListParserElement> allElements = parser.getAllElements();
//      Collections.reverse( allElements );
        
        for ( NoteListParserElement element : allElements ) {
            defineDoc( env, element );
        }
    }
    public static String nullCheck( String s ) {
        if ( s == null )
            return "";
        else 
            return s;
    }
    public static final String PREFIX = "note-event-"; 
    public static void defineDoc( Environment env, NoteListParserElement element ) {
        LamuDocument bean = new LamuDocument();
        bean.setCategory( NOTES_ID );
        bean.setSynopsisFormatter( NoteListSynopsisFormatter.getInstance() );
      
        /*
         * Generate a unique identifier from the name.
         * Prioritize its short name than its long name.    
         */
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
        
        SchemeUtils.defineVar(env, bean, "note-" + bean.getName() );
    }
}
