package lamu.lib.scheme.doc.old;

import java.util.ArrayList;
import java.util.List;

public class ProceduralDescriptiveBean extends DescriptiveBean {
    public static ProceduralDescriptiveBean create() {
        return new ProceduralDescriptiveBean();
    }
    protected ProceduralDescriptiveBean() {
        super();
    }
    public ProceduralDescriptiveBean(
            List<String> names, String parameterDescription, String returnValueDescription,
            String shortDescription, String longDescription) {
        super( names, parameterDescription, returnValueDescription, shortDescription, longDescription );
    }

    @Override
    public String formatParameter(Param param) {
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

    @Override
    public String formatParameterDescription( int seriesNo ) {
        List<String> stringList = new ArrayList<>();
        for ( Param param : this.getParameterList( seriesNo ) ) {
            stringList.add( formatParameter( param ) );
        }
        return String.join( " ", stringList );
    }

    @Override
    public String formatSynopsis(int seriesNo ) {
        return formatSynopsisForProcedure( seriesNo, (ProceduralDescriptiveBean) this );
    }
    static String formatSynopsisForProcedure( int seriesNo, ProceduralDescriptiveBean bean ) {
        String syn = seriesNo < 0 ? "" : bean.formatParameterDescription(seriesNo);
        String rv = bean.getReturnValueDescription();
        String str = "(" +
                String.join( "|", bean.getNames()) +
                (syn.equals("") ? "" : " ") +
                syn +
                ")" + rv;
        return str;
    }
}
