package lamu.lib.scheme.doc.old;

import lamu.lib.scheme.SchemeDocument;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;

public class DescriptiveHelpProcedure extends MultipleNamedProcedure0 {
    public DescriptiveHelpProcedure(String name) {
        super(name);
    }
    @Override
    public Object apply0() throws Throwable {
        String name = this.getName();
        String s = String.format( 
                   "#| (%1$s) is a documentation procedure.\n "
                 + " | Execute (help %1$s) for further information. \n"
                 + "  |# help %1$s \n",  name );
        return SchemeDocument.makeSchemeDocument( SchemeUtils.toSchemeString( s ));
    }
}

