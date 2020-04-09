package lamu;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import gnu.expr.SourceName;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;
import gnu.mapping.Symbol;
import gnu.mapping.Values;
import lamu.lib.doc.KawapadDocumentFormatter;
import lamu.lib.doc.LamuDocument;
import lamu.lib.doc.LamuDocumentFormatterUtil;
import lamu.lib.scheme.SchemeDocument;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.scheme.doc.DescriptiveHelp;
import lamu.lib.scheme.proc.MultipleNamedProcedure1;
import lamu.lib.scheme.proc.MultipleNamedProcedure2;
import lamu.lib.scheme.proc.MultipleNamedProcedureN;

public class help {
    private static final String NO_DOCUMENT_IS_AVAILABLE = "no-document-is-available";
    private static int helpTextWidth = 60;
    public static int getHelpTextWidth() {
        return helpTextWidth;
    }
    public static void setHelpTextWidth(int helpTextWidth) {
        help.helpTextWidth = helpTextWidth;
    }
    
    @SourceName( name = "make-page")
    public static Procedure makePageProc = new MakePageProc( "make-page" );
    public static final class MakePageProc extends Procedure1 {
        public MakePageProc(String name) {
            super(name);
        }
        @Override
        public Object apply1(Object arg1) throws Throwable {
            return SchemeDocument.makeSchemeDocument( 
                        SchemeUtils.toSchemeString( 
                            LamuDocumentFormatterUtil.formatKawapad( SchemeUtils.anyToString(arg1), helpTextWidth ) )); 
        }
    }

    static final LamuDocument noDocAvailableDoc = new LamuDocument(){{
        setVisible( false );
        setCategory("help-procedures");
        setNames( NO_DOCUMENT_IS_AVAILABLE );
        setReturnValueDescription( "::void" );
        setShortDescription( "No document is available." );
        setLongDescription( ""
                            + "No document is available." );
    }};

    static final LamuDocument makePageDoc = new LamuDocument(){{
        setCategory("help-procedures");
        setNames( "make-page" );
        setParameterDescription( "[string]" );
        addParameter( 0, "content",   "string",  null,  false, "the content to convert to a ||kawapad-page||. " );
        setReturnValueDescription( "::kawapad-page" );
        setShortDescription( "<name/> makes the passed value into ||kawapad-page|| object. " );
        setLongDescription( ""
                            + "When an expression is evaluated in Kawapad, the result value is displayed on the current editor. "
                            + "When the result value is a ||kawapad-page|| object, the value is displayed in a special way; "
                            + "when the Kawapad system detect the result value is a ||kawapad-page||, the editor expands the current "
                            + "selection to the outer-most parentheses and replace the region with the result value. "
                            + "This enables it to use Kawapad as a dynamic Hypertext editor. \n\n"
                            + "The <name/> procedure convert the passed value into the kawapad-page object in order to "
                            + "activate the special display function of Kawapad. "
                            + "" 
                            );
    }};
    
    @SourceName( name = "help")
    public static final HelpProc helpProc = new HelpProc( "help" );
    public static final class HelpProc extends MultipleNamedProcedure1 {
        public HelpProc( String ... names ) {
            super( names );
        }
        @Override
        public Object apply1(Object arg1) throws Throwable {
            LamuDocument doc;
            if ( arg1 instanceof LamuDocument ) {
                doc= (LamuDocument)arg1;
            } else if ( arg1 instanceof Procedure ) {
                List<LamuDocument> documentList = LamuDocument.get( LamuDocument.createConditionByProcedure( (Procedure) arg1 ) );
                if ( documentList.isEmpty() ) {
                    doc = LamuDocument.get( LamuDocument.createConditionByName( NO_DOCUMENT_IS_AVAILABLE )).get(0);
                } else if ( 1 < documentList.size() ) {
                    doc = documentList.get(0);
//                    throw new InternalError( "found duplicated documents" + documentList );
                } else {
                    doc = documentList.get(0);
                }
            } else { 
                throw new IllegalArgumentException( "invalid argument error (" + arg1 + ")"  );
            }

            return SchemeDocument.makeSchemeDocument(
                SchemeUtils.toSchemeString(
                    KawapadDocumentFormatter.getInstance().format( doc )));
        }
    }
    
    static final LamuDocument helpDoc =  new LamuDocument(){{
        setCategory( "help-procedures" );
        setNames( "help", "he" );
        setParameterDescription( "[symbol|procedure]" );
        addParameter( 0, 
            "query" , "'procs|'notes|'all|procedure", "'all", false, "" );
        
        setReturnValueDescription( "::string|list" );
        setShortDescription( "is a procedure to show the description of a specified procedure." ); 
        setLongDescription( 
                "When a reference to a procedure is passed, ||<name/>|| returns "
                + "the description of the the procedure. \n\n"
                + "If no procedure is specified, it returns a list that contains all procedures which "
                + "description is available. "
                + "Pass a special keyword 'all to get "
                + "a symbol list of all procedures which are available for this command. "
                + "Pass 'procs to get all available procedures. "
                + "Pass 'notes to get all available notation types. " 
                );
    }};
    
    @SourceName(name="help!")
    public static final HelpBangProc helpBang = new HelpBangProc( "help!" );  
    public static final class HelpBangProc extends MultipleNamedProcedureN {
        public HelpBangProc(String... names) {
            super(names);
        }
        @Override
        public Object applyN(Object[] args) throws Throwable {
            return "Calm down!";
        }
    }
    
    static final LamuDocument helpBangDoc =  new HelpBangDoc();
    static final class HelpBangDoc extends LamuDocument {
        {
            setCategory( "help-procedures" );
            setNames( "help!" );
            setParameterDescription( "" );
            setReturnValueDescription( "::string" );
            setShortDescription(  "is a procedure to execute when the user needs something which calms you down." );
            setLongDescription( 
                "When this procedure is called, this procedure will return a message which "
                + "tries to calm the user down. Any argument specified to this procedure will be silently ignored. "
                + "This procedure is deliberately defined as a joke and has by no means effect to the current system state "
                + "nor any other related elements. Note that calling this method does not cause any side effect "
                + " which is specified in Scheme name convensions specifies in https://www.scheme.com/tspl2d/intro.html "
                + "in spite of the fact the name of this method is with an exclamation mark. "
                + " "
                + "See (help about-main)." );
        }
    }


    @SourceName( name = "make-help")
    public static final Procedure makeHelp = new MakeHelpProc( "make-help" );
    public static final class MakeHelpProc extends MultipleNamedProcedure2 {
        Symbol names = Symbol.valueOf( "names" );
        Symbol params = Symbol.valueOf( "params" );
        Symbol returns = Symbol.valueOf( "returns" );
        Symbol shortDescription = Symbol.valueOf( "short-description" );
        Symbol longDescription = Symbol.valueOf( "long-description" );

        public MakeHelpProc(String ... names) {
            super(names);
        }

        @Override
        public Object apply2(Object arg1,Object arg2) throws Throwable {
            LList list = (LList) arg2;
            LamuDocument bean = new LamuDocument();
            for ( Object e : list ) {
                Pair ep = (Pair) e;
                Object car = ep.getCar();
                Object cdr = ep.getCdr();
                if ( names.equals( car ) ) {
                    
                    bean.setNames((List<String>)
                        new ArrayList((LList)cdr)
                            .stream()
                            .map((e2)->SchemeUtils.toString(e2))
                            .collect(Collectors.toList())
                            );
                    
                } else if ( params.equals( car ) ) {
                    if ( ! ( cdr instanceof LList ) ) {
                        throw new IllegalArgumentException( "`param` paramer should be a three-dimensional list" );
                    }
                    
                    for ( Object o3: ((LList)cdr) ) {
                        if ( ! ( o3 instanceof LList ) ) {
                            throw new IllegalArgumentException( "every element in the `param` argument must be a two-dimensional list" );
                        }

                        int seriesNo = -1;
                        for ( Object o4: ((LList)o3)) {
                            if ( seriesNo < 0 ) {
                                if ( ! SchemeUtils.isQuantity( o4 ) ) {
                                    throw new IllegalArgumentException( "the first element in a list in the param paramer must be a number" );
                                }
                                seriesNo = SchemeUtils.toInteger( o4 );
                                continue;
                            } else {
                                if ( ! ( o4 instanceof LList ) ) {
                                    throw new IllegalArgumentException( "every element except the first in a list in the param paramer must be a list"  );
                                }
                            }
                            
                            if ( seriesNo < 0 ) {
                                throw new IllegalArgumentException( "series number is not set" );
                            }

                            LList l4=(LList)o4;
                            if ( l4.size() != 5 ) {
                                throw new IllegalArgumentException("the length in an element in 'params' parameter must be a list which size is 5. ");
                            }
                            bean.addParameter( 
                                seriesNo,                            // series number 
                                SchemeUtils.toString ( l4.get( 0 )), // names
                                SchemeUtils.toString ( l4.get( 1 )), // type, 
                                Boolean.FALSE.equals ( l4.get( 2 )) ?
                                        null :
                                            SchemeUtils.toString(l4.get( 2 )), // defaultValue,
                                            SchemeUtils.toBoolean( l4.get( 3 )), // isVariable,
                                            SchemeUtils.toString ( l4.get( 4 ))  //description );
                                    );
                        }

                    }
                } else if ( returns.equals( car ) ) {
                    bean.setReturnValueDescription( SchemeUtils.toString( ((Pair)cdr).getCar() ));
                } else if ( shortDescription.equals( car ) ) {
                    bean.setShortDescription( SchemeUtils.toString(
                        String.join( "", 
                            SchemeUtils.anySchemeValueListToStringList(
                                ((Pair)cdr)))));
                } else if ( longDescription.equals( car ) ) {
                    bean.setLongDescription( SchemeUtils.toString( 
                        String.join( "", 
                            SchemeUtils.anySchemeValueListToStringList(
                                ((Pair)cdr)))));
                } else {
                    throw new IllegalArgumentException( "unknown field name " + car );
                }
            }
            return Values.empty;
        }
    }
    
    static final LamuDocument makeHelpDoc = new LamuDocument(){{
        setCategory( "help-procedures" );
        setNames( "make-help" );
        setParameterDescription( "" );
        addParameter( 0, "target", "procedure" , null, false, "The reference to the target procedure. See the description. " );
        addParameter( 0, "content", "(list cons ...)" , null, false, "See the description. " );
        setReturnValueDescription( "::void" );
        setShortDescription(  "||<name/>|| registers a reference manual for a procedure on the Pulsar documentation system. " );
        setLongDescription( 
            " "
            + "The ||target|| argument is the reference of the target procedure."
            + "The ||content|| argument is the content of the reference manual. The value is "
            + "an association list contains various data. \n\n"
            + "    (<name/>   target-proc"
            + "               '((names \"foo-bar\" \"fb\") \n"
            + "               (params\n"
            + "                 (\"param-name\" \"param-type\" \"default-value or #f if no-default\" \"#t if variable-length\" \"description\") \n"
            + "                    ...\n"
            + "                 )\n"
            + "                (returns \"return-type\" )\n"
            + "                (short-description \"description\" )\n"
            + "                (long-description  \"description\" )\n"
            + "              )\n\n"
            + "The ||name|| field contains names of the procedure. "
            + "In Pulsar, the most procedures have multiple names. "
            + "The first element of this list is its 'long name' which should be the canonical name for the procedure. "
            + "And the others are its aliases. If the procedure have no alias, then the list will have only one element. "
            + "The list must have at least one element. \n\n"
            + "The ||params|| field contains information of parameters. "
            + "The field contains a list per a parameter. \n\n"
            + "The ||short-description|| field contains a string value of its short description. "
            + "The ||long-description|| field contains a string value of its long description. "
            + "" );
    }};
    
    public static final HelpMarkdownProc helpMarkdownProc = new HelpMarkdownProc( "help-markdown" );
    public static final class HelpMarkdownProc extends MultipleNamedProcedure1 {
        public HelpMarkdownProc( String ... names ) {
            super(names);
        }
        @Override
        public Object apply0() throws Throwable {
            return this.apply1( DescriptiveHelp.DOCS.getSymbol() );
        }
        @Override
        public Object apply1(Object arg1) throws Throwable {
//            // TODO
//            Environment env=null;
//            System.out.println(
//                outputMarkdownReference0(
//                    DescriptiveDocumentCategory.valueOf((Symbol)arg1), env));
////              SchemeUtils.toSchemeSymbol( sb.toString() );
            return Values.empty;
        }
    }
    
    public static final LamuDocument helpMarkdownDoc = new LamuDocument(){{
        setCategory( "help-procedures" );
        setNames("help-markdown");
        setParameterDescription( "" );
        addParameter( 0, "type", "string", "'procs", false,  "either 'procs or 'notes " );
        setReturnValueDescription( "::string" );
        setShortDescription(  "is a procedure to execute when the user needs something which calms you down." );
        setLongDescription( 
            "When this procedure is called, this procedure will return a message which "
            + "tries to calm the user down. Any argument specified to this procedure will be silently ignored."
            + "This procedure is deliberately defined as a joke and has by no means effect to the current system state "
            + "nor any other related elements. See (help about-main)." );
    }};
}
