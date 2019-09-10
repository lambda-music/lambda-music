package pulsar.lib.scheme;

public interface KawapadDescriptive {
    public static String formatForKawapad( DescriptiveBean bean ) {
        StringBuilder message = new StringBuilder();
        
        message.append( "========== THE MANUAL OF PULSAR LISP SCHEME MUSIC SEQUENCER =========\n\n" );
        message.append( "NAME: " );
        message.append( bean.getName().toUpperCase()  );
        message.append( "\n\n" );
        {
            String syn = bean.formatParameterDescription();
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
