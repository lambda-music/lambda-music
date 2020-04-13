package lamu.lib.doc;

import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MarkdownDocumentFormatter implements LamuDocumentFormatter {
    @Override
    public String format(LamuDocument document) {
        if ( document == null )
            throw new NullPointerException( "bean is null" );
        
        StringBuilder message = new StringBuilder();
        
        String name = document.getName().toUpperCase();

//      message.append( "========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========\n\n" );
        message.append( "" + name + "\n" );
        message.append( "====================" );
        message.append( "\n\n" );
        
        if ( document.getParameterListCount() == 0 ) {
            message.append( 
                    "#### SYNOPSIS ####\n" );
            
        }
        for ( int seriesNo=0; seriesNo< document.getParameterListCount(); seriesNo++ ) {
            message.append( 
                    "#### SYNOPSIS ####\n" );
            message.append( "    " );
            message.append( document.getSynopsis( seriesNo ));
            message.append( "\n\n" );
        }

        String msg1;
        {
            msg1 = document.getShortDescription();
            msg1 = document.interporlate( msg1 );
            msg1 = escapeMarkdown( msg1 );
        }

        String msg2;
        {
            msg2 = document.getLongDescription();
            msg2 = document.interporlate( msg2 );
            msg2 = escapeMarkdown( msg2 );
        }
        
        message.append( "### DESCRIPTION ###\n" );
        message.append( wrapMultiLine( msg1 + " " + msg2 , 80 ) );
        message.append( "\n\n" );
        message.append( "--------------------------------------------------------" );
        message.append( "" );

        return message.toString();  
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    private static final Pattern IS_INDENTED= Pattern.compile( "^\\s+" );
    protected static final String wrapMultiLine( String s, int width ) {
        StringBuilder sb = new StringBuilder();
//      String[] a = s.split("\\r\\n\\r\\n|\\n\\r\\n\\r|\\n\\n|\\r\\r" );
        String[] a = s.split( "\n\n" );
        for ( int i=0; i<a.length; i++ ) {
            // if the line starts with blank characters (that is, the line is indented),
            // then leave it as it is; otherwise, wrap it with the specified length.
            // (Thu, 29 Aug 2019 05:58:14 +0900)
            if ( IS_INDENTED.matcher( a[i] ).find()) {
                sb.append(      a[i]               ).append( "\n\n" );
            } else {
//              System.err.println( "SDDDD" );
                sb.append( wrap(a[i],width).trim() ).append( "\n\n" );
            }
        }
        return sb.toString();
    }
    
    protected static final String prefixMultiLine( String s, String prefix ) {
        StringBuilder sb = new StringBuilder();
        String[] a = s.split( "\n" );
        for ( int i=0; i<a.length; i++ ) {
             sb.append(prefix).append( a[i] ).append( "\n" );
        }
        return sb.toString();
    }
    
    protected static String wrap( String s, int width ) {
        Matcher m = Pattern.compile( "\\s+" ).matcher(s);
        StringBuilder sb = new StringBuilder();
        int head=0;
        int last=head;
        while ( m.find() ) {
            int curr = m.start();
            String stringToAdd = s.substring(last,curr);
            if ( ( curr-head ) < width ) {
                sb.append(' ');
                sb.append( stringToAdd );
                last = curr + m.group().length();
            } else {
                sb.append('\n');
                sb.append( stringToAdd );
                last = curr + m.group().length();
                head = last;
            }
        }
        {
            String stringToAdd = s.substring(last,s.length());
            sb.append(' ');
            sb.append( stringToAdd );
            sb.append('\n');
        }
        return sb.toString();
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //////////////////////////////////////////////////////////////////////////////////////////////////////


    public static String createMarkdownHelp( List<LamuDocument> documentList ) {
        StringBuilder sb = new StringBuilder();
        Collections.reverse( documentList );
        for ( LamuDocument document : documentList ) {
//          System.out.println( o );
            sb.append( MarkdownDocumentFormatter.formatForMarkdown( document ) );
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
    public static String formatForMarkdown( LamuDocument bean ) {
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
            message.append( bean.getSynopsis( seriesNo ));
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
        message.append( wrapMultiLine( msg1 + " " + msg2 , 80 ) );
        message.append( "\n\n" );
        message.append( "--------------------------------------------------------" );
        message.append( "" );

        return message.toString();  
    }


    
}
