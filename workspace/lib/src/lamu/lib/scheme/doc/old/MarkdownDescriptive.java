package lamu.lib.scheme.doc.old;

import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import gnu.lists.Pair;
import gnu.mapping.Procedure;
import lamu.lib.doc.LamuDocumentFormatterUtil;

public class MarkdownDescriptive {
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

    static Pattern pe  = Pattern.compile( "[\\\\\\[\\]\\#\\(\\)\\*\\`\\_\\{\\}]", Pattern.MULTILINE );
    static Pattern pe2 = Pattern.compile( "^\\s+" );
    public static void main(String[] args) {
        System.out.println( pe2.matcher( "   hello" ).find() );
        System.out.println( pe2.matcher( "hello" ).find() );
        System.out.println( pe2.matcher( "returns the current open state." ).find() );
         
    }
    static String escapeMarkdown( String s ) {
        String[] ss = s.split( "\\n" );
        for ( int i=0; i<ss.length; i++ ) {
            if ( ! pe2.matcher( ss[i] ).find() ) {
                ss[i] = pe.matcher( ss[i] ).replaceAll( "\\\\$0" );
            }
        }
        return String.join( "\n", ss );
    }

    static Pattern p1 = Pattern.compile( "([\\(\\)\\[\\]])" );
    public static String formatForMarkdown( DescriptiveBean bean ) {
        if ( bean == null )
            throw new NullPointerException( "bean is null" );
        
        StringBuilder message = new StringBuilder();
        
        String name = bean.getName().toUpperCase();

//      message.append( "========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========\n\n" );
        message.append( "" + name + "\n" );
        message.append( "====================" );
        message.append( "\n\n" );
        
        if ( bean.getParameterListCount() == 0 ) {
            message.append( 
                    "#### SYNOPSIS ####\n" );
            
        }
        for ( int seriesNo=0; seriesNo< bean.getParameterListCount(); seriesNo++ ) {
            message.append( 
                    "#### SYNOPSIS ####\n" );
            message.append( "    " );
            message.append( bean.formatSynopsis(seriesNo));
            message.append( "\n\n" );
        }

        String msg1;
        {
            msg1 = bean.getShortDescription();
            msg1 = bean.interporlate( msg1 );
            msg1 = escapeMarkdown( msg1 );
        }

        String msg2;
        {
            msg2 = bean.getLongDescription();
            msg2 = bean.interporlate( msg2 );
            msg2 = escapeMarkdown( msg2 );
        }
        
        message.append( "### DESCRIPTION ###\n" );
        message.append( LamuDocumentFormatterUtil.wrapMultiLine( msg1 + " " + msg2 , 80 ) );
        message.append( "\n\n" );
        message.append( "--------------------------------------------------------" );
        message.append( "" );

        return message.toString();  
    }
    public static String formatForMarkdown( Procedure p ) {
        return formatForMarkdown( DescriptiveProcedure.getDescriptionBean( p ) );
    }
}
