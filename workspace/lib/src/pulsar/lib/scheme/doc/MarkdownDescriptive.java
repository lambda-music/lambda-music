package pulsar.lib.scheme.doc;

import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import gnu.lists.Pair;
import gnu.mapping.Procedure;

public class MarkdownDescriptive extends Descriptive {
    MarkdownDescriptive() {
    }
    public static String createMarkdownHelp( List<Object> list ) {
        StringBuilder sb = new StringBuilder();
        Collections.reverse( list );
        for ( Object o : list ) {
//          System.out.println( o );
            sb.append( MarkdownDescriptive.formatForMarkdown( (Procedure)((Pair)o).getCar() ) );
            sb.append( "\n\n" );
        }
        return sb.toString();
    }

    static Pattern p1 = Pattern.compile( "([\\(\\)\\[\\]])" );
    public static String formatForMarkdown( DescriptiveBean bean ) {
        StringBuilder message = new StringBuilder();
        
        String name = bean.getName().toUpperCase();

//      message.append( "========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========\n\n" );
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
        message.append( wrapMultiLine( msg1 + " " + msg2 , 80 ) );
        message.append( "\n\n" );
        message.append( "--------------------------------------------------------" );
        message.append( "" );

        return message.toString();  
    }
    public static String formatForMarkdown( Procedure p ) {
        return formatForMarkdown( getDescriptionBean( p ) );
    }
}
