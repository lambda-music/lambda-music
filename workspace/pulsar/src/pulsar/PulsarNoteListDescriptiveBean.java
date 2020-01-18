package pulsar;

import java.util.ArrayList;
import java.util.List;

import pulsar.lib.scheme.doc.DescriptiveBean;

public class PulsarNoteListDescriptiveBean extends DescriptiveBean {
    public PulsarNoteListDescriptiveBean() {
        super();
    }
    public PulsarNoteListDescriptiveBean(
            List<String> names, String parameterDescription, String returnValueDescription,
            String shortDescription, String longDescription) {
        super( names, parameterDescription, returnValueDescription, shortDescription, longDescription );
    }
    @Override
    public String formatParameter(Param param) {
        String result = "[" + String.join( "|", param.getNames() ) + "]: " + param.getType() ;
        if ( param.getDefaultValue() != null )
            result += "=" + param.getDefaultValue(); 
        return result;
    }
    @Override
    public String formatParameterDescription(int seriesNo) {
        StringBuilder sb = new StringBuilder();
        List<String> stringList = new ArrayList<>();
        for ( int i=0; i<this.getParameterListCount(); i++ ) {
            for ( Param param : this.getParameterList( i ) ) {
                stringList.add( formatParameter( param ) );
            }
            sb.append( String.join( " ", stringList ) );
            sb.append( "\n" );
        }
        return sb.toString();
    }
    @Override
    public String formatSynopsis( int seriesNo ) {
        return formatSynopsisForNoteListParserElement( seriesNo, this );
    }
    static String formatSynopsisForNoteListParserElement( int seriesNo, DescriptiveBean bean ) {
        String syn = bean.formatParameterDescription( seriesNo );
        String rv = bean.getReturnValueDescription();
        String str = "(n type: '[" +
                String.join( "|", bean.getNames()) +
                "]" +
                (syn.equals("") ? "" : " ") +
                syn +
                ")" + rv;
        return str;
    }

}
