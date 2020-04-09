package pulsar;

import java.util.Arrays;
import java.util.List;

import gnu.mapping.Environment;
import lamu.lib.doc.LamuDocument;
import lamu.lib.doc.NoteListSynopsisFormatter;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.scheme.doc.DescriptiveDocumentCategory;

public class PulsarDocuments {
    public static final DescriptiveDocumentCategory DOCS = 
    		DescriptiveDocumentCategory.createCategory( 
    				"pulsar-procedures",
    				new Runnable() {
		    			@Override
		    			public void run() {
		    				// Note : (Sat, 07 Mar 2020 23:38:27 +0900)
		    				// 
		    				// For now, this should be done by Scheme initializer; therefore, this
		    				// is not necessary to initializer here. In near future, Pulsar.initScheme()
		    				// method is possiblly called by Kawa's 'require'; in such case, the following
		    				// initialization must be done.
		    				// 
		    				// Pulsar.initScheme( SchemeEvaluator.getCurrent().getScheme() );
		    				// 
		    			}
		    		});

    public static final DescriptiveDocumentCategory NOTES = 
            DescriptiveDocumentCategory.createCategory( "pulsar-notations", new Runnable() {
            	@Override
            	public void run() {
    				// Note : (Sat, 07 Mar 2020 23:38:27 +0900)
            		// The situation is same as the statement above.  
            	}
            });

    private PulsarDocuments() {}
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
        bean.setCategory( "pulsar-notations" );
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
