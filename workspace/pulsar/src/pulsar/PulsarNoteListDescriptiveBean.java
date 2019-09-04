package pulsar;

import java.util.ArrayList;
import java.util.List;

import pulsar.lib.scheme.DescriptiveBean;

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
    public String formatParameterDescription() {
        List<String> stringList = new ArrayList<>();
        for ( Param param : this.getParameterList() ) {
            stringList.add( formatParameter( param ) );
        }
        return String.join( " ", stringList );
    }
    @Override
    public String formatSynopsis() {
        return formatSynopsisForNoteListParserElement( this );
    }
    static String formatSynopsisForNoteListParserElement(DescriptiveBean bean) {
        String syn = bean.formatParameterDescription();
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
