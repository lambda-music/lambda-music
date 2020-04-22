package lamu.lib.helps;

import java.util.ArrayList;
import java.util.List;

import lamu.lib.helps.LamuDocument.Param;

public class ProcedureSynopsisFormatter implements LamuDocumentSynopsisFormatter {
    private static final ProcedureSynopsisFormatter INSTANCE = new ProcedureSynopsisFormatter();
    public static ProcedureSynopsisFormatter getInstance() {
        return INSTANCE;
    }
    static String formatParameter(Param param) {
        String result = String.join( "|", param.getNames() ) + "::" + param.getType();
        if ( param.getDefaultValue() != null ) {
            result =  "[" + result + "=" + param.getDefaultValue() +  "]";
            if ( param.isVariable() ) {
                result =  result + "...";
            }
        } else {
            if ( param.isVariable() ) {
                result =  "[" + result + "]...";
            }
        }
        return result;
    }
    
    static String formatParameterDescription( List<Param> parameterList ) {
        List<String> stringList = new ArrayList<>();
        for ( Param param : parameterList ) {
            stringList.add( formatParameter( param ) );
        }
        return String.join( " ", stringList );
    }
    static String formatSynopsisForProcedure( LamuDocument bean, int seriesNo ) {
        String syn = seriesNo < 0 ? "" : formatParameterDescription( bean.getParameterList(seriesNo));
        String rv = bean.getReturnValueDescription();
        String str = "(" +
                String.join( "|", bean.getNames()) +
                (syn.equals("") ? "" : " ") +
                syn +
                ")" + rv;
        return str;
    }
    @Override
    public String getSynopsis( LamuDocument document, int seriesNo ) {
        return formatSynopsisForProcedure( document, seriesNo );
    }
}
