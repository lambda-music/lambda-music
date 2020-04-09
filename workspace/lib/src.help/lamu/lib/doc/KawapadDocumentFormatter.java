package lamu.lib.doc;

public class KawapadDocumentFormatter implements LamuDocumentFormatter {
    private static final KawapadDocumentFormatter INSTANCE = new KawapadDocumentFormatter();
    public static KawapadDocumentFormatter getInstance() {
        return INSTANCE;
    }
    @Override
    public String format(LamuDocument document) {
        StringBuilder message = new StringBuilder();
        
        message.append( "========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========\n\n" );
        message.append( "NAME: " );
        message.append( document.getName().toUpperCase()  );
        message.append( "\n\n" );
        for ( int seriesNo=0; seriesNo< document.getParameterListCount(); seriesNo++ ) {
            message.append( "SYNOPSIS:" ).append( document.getSynopsis( seriesNo ) );
            
//            String rv = document.getReturnValueDescription();
//            message.append( 
//                    "SYNOPSIS: (" +
//                    String.join( "|", 
//                        document.getNames()) +
//                    (syn.equals("") ? "" : " ") +
//                    syn +
//                    ")" + rv);
            
            message.append( "\n\n" );
        }

        String msg1;
        {
            msg1 = document.getShortDescription();
            msg1 = document.interporlate( msg1 );
        }

        String msg2;
        {
            msg2 = document.getLongDescription();
            msg2 = document.interporlate( msg2 );
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
