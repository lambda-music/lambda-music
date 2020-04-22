package lamu.lib.helps;

import java.util.ArrayList;
import java.util.List;

public class NoteListSynopsisFormatter implements LamuDocumentSynopsisFormatter {
    private static final NoteListSynopsisFormatter INSTANCE = new NoteListSynopsisFormatter();
    public static NoteListSynopsisFormatter getInstance() {
        return INSTANCE;
    }
    private NoteListSynopsisFormatter() {
    }
    public static String formatParameter(LamuDocument.Param param) {
        String result = "[" + String.join( "|", param.getNames() ) + "]: " + param.getType() ;
        if ( param.getDefaultValue() != null )
            result += "=" + param.getDefaultValue(); 
        return result;
    }
    
    public static String formatParameterDescription( LamuDocument document ) {
        StringBuilder sb = new StringBuilder();
        List<String> stringList = new ArrayList<>();
        for ( int i=0; i<document.getParameterListCount(); i++ ) {
            for ( LamuDocument.Param param : document.getParameterList( i ) ) {
                stringList.add( formatParameter( param ) );
            }
            sb.append( String.join( " ", stringList ) );
            sb.append( "\n" );
        }
        return sb.toString();
    }
    
    static String formatSynopsisForNoteListParserElement( LamuDocument bean, int seriesNo ) {
        String syn = seriesNo < 0 ? "" : formatParameterDescription( bean ) ;
        String rv = bean.getReturnValueDescription();
        String str = "(n type: '[" +
                String.join( "|", bean.getNames()) +
                "]" +
                (syn.equals("") ? "" : " ") +
                syn +
                ")" + rv;
        return str;
    }

    @Override
    public String getSynopsis( LamuDocument document, int seriesNo ) {
        return formatSynopsisForNoteListParserElement( document, seriesNo );
    }
}
