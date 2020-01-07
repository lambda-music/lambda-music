package pulsar.lib.scheme.doc;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import gnu.lists.EmptyList;
import gnu.lists.LList;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Procedure1;
import gnu.mapping.Procedure2;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import gnu.mapping.Values;
import gnu.mapping.WrongArguments;
import kawa.standard.Scheme;
import pulsar.lib.scheme.SafeProcedureN;
import pulsar.lib.scheme.SchemeEngine;
import pulsar.lib.scheme.SchemeEvaluator.SchemeEngineListener;
import pulsar.lib.scheme.SchemeUtils;

public class DescriptiveHelp {
    public static final DescriptiveDocumentCategory DOCS = 
            DescriptiveDocumentCategory.createCategory( "help-procedures" );
    /**
     * This initializes variables which do not need to refer the reference to the
     * current frame. This initializer does not have to be removed even if  
     * frames are disposed.
     */
    public static void registerGlobalSchemeInitializer( SchemeEngine schemeEngine ) {
        schemeEngine.getSchemeEvaluator().registerSchemeInitializer( initSchemeListener );
    }

    static SchemeEngineListener initSchemeListener = new SchemeEngineListener() {
        @Override
        public void execute( Scheme scheme ) {
            DescriptiveHelp.initScheme( scheme.getEnvironment() );            
        }
    };

    /**
     * 
     */

    static String outputMarkdownReference0(DescriptiveDocumentCategory type, Environment env ) {
        List list = new ArrayList();
        list.addAll( type.getDocumentList( env ));
        return MarkdownDescriptive.createMarkdownHelp( list );
    }
    public static String outputMarkdownReference( DescriptiveDocumentCategory type, SchemeEngine schemeEngine ) {
        if ( type == null )
            throw new IllegalArgumentException( "'type' argument cannot be null." );
        
        return outputMarkdownReference0( type, schemeEngine.getSchemeEvaluator().getScheme().getEnvironment() );
    }
    static int helpTextWidth = 60;

    public static void initScheme( Environment env ) {
        
        ///////////////////////////////////////////////////////////////////////////////////////////////
        
        SchemeUtils.defineVar( env, new Procedure1("make-page") {
            @Override
            public Object apply1(Object arg1) throws Throwable {
                return Descriptive.makeSchemeDocument( 
                            SchemeUtils.toSchemeString( 
                                Descriptive.formatKawapad( SchemeUtils.anyToString(arg1), helpTextWidth ) )); 
            }
        }, "make-page");
        
        DescriptiveHelp.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
            setNames( "make-page" );
            setParameterDescription( "[string]" );
            addParameter("content",   "string",  null,  false, "the content to convert to a ||kawapad-page||. " );
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
        }} );
        
        SchemeUtils.defineVar( env, new SafeProcedureN("help!") {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                return "Calm down!";
            }
        }, "help!");
        
        DescriptiveHelp.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
            setNames( "help!" );
            setParameterDescription( "" );
            setReturnValueDescription( "::string" );
            setShortDescription(  "is a procedure to execute when the user needs something which calms you down." );
            setLongDescription( 
                "When this procedure is called, this procedure will return a message which "
                + "tries to calm the user down. Any argument specified to this procedure will be silently ignored."
                + "This procedure is deliberately defined as a joke and has by no means effect to the current system state "
                + "nor any other related elements. See (help about-main)." );
        }} );
        
        final class ProcedureHelp extends SafeProcedureN {
            final Environment environment;
            final int index;
            final Procedure reverse = (Procedure)gnu.kawa.slib.srfi1.reverse.get();
            final Procedure map = (Procedure)gnu.kawa.slib.srfi1.map.get();
            
            private ProcedureHelp(Environment environment, String name, int index ) {
                super(name);
                this.environment=environment ;
                this.index = index;
            }
            
            LList getAvailableProcedures( DescriptiveDocumentCategory category ) throws Throwable {
                Procedure1 proc1 = new Procedure1() {
                    @Override
                    public Object apply1(Object arg1) throws Throwable {
                        Pair pair = (Pair)arg1;
                        Object result;
                        if ( index < pair.length() ) {
                            result = pair.get(index);
                        } else if ( 1 < pair.length() ) {
                            result =  pair.get(1);
                        } else if ( 0 < pair.length() ) {
                            result =  Symbol.valueOf(((Procedure)pair.get(0)).getName());
                        } else {
                            result =  "";
                        }
                        
                        return result;
                    }
                };
                return (LList)map.apply2( proc1, 
                                reverse.apply1( 
                                    category.getDocumentList( this.environment )));
            }
            ArrayList getAllAvailableProcedures() throws Throwable {
                ArrayList list = new ArrayList();
                for ( DescriptiveDocumentCategory category : DescriptiveDocumentCategory.getAll() ) {
                    list.addAll( getAvailableProcedures( category ) );
                }
                return list;
            }

            LList getAllAvailableProcedureLList() throws Throwable {
                return LList.makeList( getAllAvailableProcedures() );
            }

            Object helpList( LList list ) throws Throwable {
                Procedure1 proc1 = new Procedure1() {
                    @Override
                    public Object apply1(Object arg1) throws Throwable {
                        return SchemeUtils.toSchemeString( "(help " + SchemeUtils.schemeSymbolToJavaString( arg1 ) + ")" );
                    }
                };
                
                Object result = kawa.standard.append.append.apply2(
                                    Pair.make( 
                                        SchemeUtils.toSchemeString( "(#| === The list of all available procedures ===\n\n" ), 
                                        map.apply2( proc1, list )),
                                    Pair.make( SchemeUtils.toSchemeString( "|#)" ), EmptyList.emptyList ));
                return result;
            }; 

            public Object apply0() throws Throwable {
                return Descriptive.makeSchemeDocument( helpList( getAllAvailableProcedureLList() ) );
            }


            String MSG_NO_DOCUMENTATION = "No documentation is available.";
            SimpleSymbol ALL_AVAILABLE = Symbol.valueOf( "all" );
            public Object apply1(Object arg1) throws Throwable {
                if ( ALL_AVAILABLE.equals( arg1 ) ) {
                    return getAllAvailableProcedureLList();
                } else  {
                    if ( arg1 instanceof Symbol ) {
                        DescriptiveDocumentCategory t = 
                                DescriptiveDocumentCategory.valueOf( (Symbol)arg1 );
                        return Descriptive.makeSchemeDocument( 
                            helpList( getAvailableProcedures( t ) ) );
                    } else {
                        String message = Descriptive.getDescription( arg1 );
                        if ( message == null ) {
                            message = MSG_NO_DOCUMENTATION;
                        }
                        System.err.println( message );
                        return Descriptive.makeSchemeDocument( 
                                    SchemeUtils.toSchemeString( 
                                        Descriptive.formatKawapad( message, helpTextWidth )));
                                
                    }
                }
            }


            @Override
            public Object applyN(Object[] args) throws Throwable {
                if ( args.length == 0 )
                    return apply0();
                else if ( args.length == 1 )
                    return apply1(args[0]);
                else
                    throw new WrongArguments( this, args.length );

            }
        }
        
        SchemeUtils.defineVar( env, new ProcedureHelp( env, "help", 1 ) , "help", "he" );
        DescriptiveHelp.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
            setNames( "help", "he" );
            setParameterDescription( "[symbol|procedure]" );
            addParameter(
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
        }} );

        
        SchemeUtils.defineVar( env, new Procedure2("make-help") {
            Symbol names = Symbol.valueOf( "names" );
            Symbol params = Symbol.valueOf( "params" );
            Symbol returns = Symbol.valueOf( "returns" );
            Symbol shortDescription = Symbol.valueOf( "short-description" );
            Symbol longDescription = Symbol.valueOf( "long-description" );
            @Override
            public Object apply2(Object arg1,Object arg2) throws Throwable {
                LList list = (LList) arg2;
                ProceduralDescriptiveBean bean = new ProceduralDescriptiveBean();
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
                        for (Object o3: ((LList)cdr)) {
                            LList l3=(LList)o3;
                            if ( l3.size() != 5 ) {
                                throw new IllegalArgumentException("an element in 'params' parameter must be a list which size is 5. ");
                            }
                            bean.addParameter( 
                                SchemeUtils.toString(l3.get( 0 )) , // names
                                SchemeUtils.toString(l3.get( 1 )), // type, 
                                Boolean.FALSE.equals( l3.get( 2 ) ) ?
                                            null :
                                            SchemeUtils.toString(l3.get( 2 )), // defaultValue,
                                SchemeUtils.toBoolean( l3.get(3 )), // isVariable,
                                SchemeUtils.toString(l3.get( 4 )) //description );
                                );
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
                DescriptiveHelp.DOCS.defineDoc( env, arg1, bean );
                return Values.empty;
            }
        }, "make-help");
        
        DescriptiveHelp.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
            setNames( "make-help" );
            setParameterDescription( "" );
            addParameter( "target", "procedure" , null, false, "The reference to the target procedure. See the description. " );
            addParameter( "content", "(list cons ...)" , null, false, "See the description. " );
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
        }} );

        
        SchemeUtils.defineVar( env, new Procedure1("help-markdown") {
            @Override
            public Object apply0() throws Throwable {
                return this.apply1( DescriptiveHelp.DOCS.getSymbol() );
            }
            @Override
            public Object apply1(Object arg1) throws Throwable {
                System.out.println(
                    outputMarkdownReference0(
                        DescriptiveDocumentCategory.valueOf((Symbol)arg1), env));
//              SchemeUtils.toSchemeSymbol( sb.toString() );
                return Values.empty;
            }

        }, "help-markdown" );
        
        DescriptiveHelp.DOCS.defineDoc( env, new ProceduralDescriptiveBean(){{
            setNames("help-markdown");
            setParameterDescription( "" );
            addParameter( "type", "string", "'procs", false,  "either 'procs or 'notes " );
            setReturnValueDescription( "::string" );
            setShortDescription(  "is a procedure to execute when the user needs something which calms you down." );
            setLongDescription( 
                "When this procedure is called, this procedure will return a message which "
                + "tries to calm the user down. Any argument specified to this procedure will be silently ignored."
                + "This procedure is deliberately defined as a joke and has by no means effect to the current system state "
                + "nor any other related elements. See (help about-main)." );
        }} );        
    }
}
