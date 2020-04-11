package lamu;

import java.awt.Color;
import java.awt.Font;
import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.logging.Level;

import javax.swing.plaf.FontUIResource;

import gnu.lists.EmptyList;
import gnu.lists.LList;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import gnu.mapping.Values;
import gnu.mapping.WrongArguments;
import kawa.standard.Scheme;
import kawapad.Kawapad;
import kawapad.KawapadFrame;
import kawapad.KawapadSyntaxElementType;
import kawapad.KawapadTextualIncrement;
import kawapad.SyntaxElement;
import lamu.lib.doc.LamuDocument;
import lamu.lib.log.Logger;
import lamu.lib.scheme.SchemeEvaluatorUtils;
import lamu.lib.scheme.SchemePrinter;
import lamu.lib.scheme.SchemeUtils;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;
import lamu.lib.scheme.proc.MultipleNamedProcedure1;
import lamu.lib.scheme.proc.MultipleNamedProcedure2;
import lamu.lib.scheme.proc.MultipleNamedProcedure3;
import lamu.lib.scheme.proc.MultipleNamedProcedureN;

public class kawapad implements Runnable {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }

    static final String FLAG_DONE_INIT_PULSAR_SCRATCHPAD = "flag-done-init-pulsar-scratchpad";

    // ( canonical )
    public static final LamuDocument aboutIntroDoc = new LamuDocument(){{
        setCategory( "kawapad-procedures" );
        setNames( "about-intro"  );
        setParameterDescription( "" );
        setReturnValueDescription( "" );
        setShortDescription( "Welcome to Kawapad!" );
        setLongDescription( ""
            + "Kawapad is a simple Lisp Scheme editor which can edit and execute Scheme code "
            + "on the fly. Kawapad includes Java implementation of a powerful computer language Lisp Scheme. "
            + " "
            + "To show all available procedures, execute (help). \n"
            + "To show help of a procedure, execute (help [procedure-name] ) . \n"
            + "" 
            );
    }};

    public static final MultipleNamedProcedure0 kawapadProc = new KawapadProc(new String[] { "kawapad" });
    public static final class KawapadProc extends MultipleNamedProcedure0 {
        public KawapadProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return Kawapad.getCurrent();
        }
    }

    public static final MultipleNamedProcedure0 isKawapadPresentProc = new IsKawapadPresentProc(new String[] { "kawapad-present?" });
    public static final class IsKawapadPresentProc extends MultipleNamedProcedure0 {
        public IsKawapadPresentProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return Kawapad.isPresent();
        }
    }

    public static final MultipleNamedProcedure0 kawapadFrameProc = new KawapadFrameProc(new String[] { "kawapad-frame" });
    public static final class KawapadFrameProc extends MultipleNamedProcedure0 {
        public KawapadFrameProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return KawapadFrame.getCurrent();
        }
    }

    public static final MultipleNamedProcedure0 isKawapadFramePresentProc = new IsKawapadFramePresentProc(new String[] { "kawapad-frame-present?" });
    public static final class IsKawapadFramePresentProc extends MultipleNamedProcedure0 {
        public IsKawapadFramePresentProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return KawapadFrame.isPresent();
        }
    }
    public static final MultipleNamedProcedure0 frameProc = new FrameProc(new String[] { "frame" });

    public static final class FrameProc extends MultipleNamedProcedure0 {
        public FrameProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return KawapadFrame.getCurrent();
        }
    }

    public static final MultipleNamedProcedure0 isFramePresentProc = new IsFramePresentProc(new String[] { "frame-present?" });
    public static final class IsFramePresentProc extends MultipleNamedProcedure0 {
        public IsFramePresentProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return KawapadFrame.isPresent();
        }
    }

    public static final MultipleNamedProcedure3 registerEventHandlerProc = new RegisterEventHandlerProc(new String[] { "register-event-handler" });
    public static final class RegisterEventHandlerProc extends MultipleNamedProcedure3 {
        public RegisterEventHandlerProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply3(Object arg1, Object arg2, Object arg3) throws Throwable {
            Kawapad.eventHandlers.register( (Symbol)arg1, (Symbol)arg2, (Procedure) arg3 );
            return EmptyList.emptyList;
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public static final MultipleNamedProcedure2 unregisterEventHandlerProc = new UnregisterEventHandlerProc( new String[] { "unregister-event-handler" });
    public static final class UnregisterEventHandlerProc extends MultipleNamedProcedure2 {
        public UnregisterEventHandlerProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply2(Object arg1, Object arg2 ) throws Throwable {
            Kawapad.eventHandlers.unregister((Symbol)arg1,(Symbol)arg2 );
            return EmptyList.emptyList;
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public static final MultipleNamedProcedure1 prettifyProc = new PrettifyProc(new String[] { "prettify", "pre" });
    public static final class PrettifyProc extends MultipleNamedProcedure1 {
        public PrettifyProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg1 ) throws Throwable {
            return SchemeUtils.toSchemeString(   
                Kawapad.correctIndentation( Kawapad.getCurrent(), 
                    SchemePrinter.printSchemeValue( arg1 )));
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public static final MultipleNamedProcedure2 loadFontProc = new LoadFontProc(new String[] { "load-font" });
    public static final LamuDocument loadFontDoc = new LamuDocument(){{
        setCategory( "kawapad-procedures" );
        setNames( "load-font"  );
        setParameterDescription( "" );
        addParameter( 0, "file-size", "string", null , false, "Specifies the path to the font file. " );
        addParameter( 0, "font-size", "number", null , false, "Specifies its font size. " );
        setReturnValueDescription( "::void" );
        setShortDescription( "Set the main font of the editor." );
        setLongDescription( ""
            + "Kawapad can change its font-face. ||<name/>|| loads a file from the filesystem and "
            + "set it to the font-face of Kawapad. "
            + "" 
            );
    }};

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public static final class LoadFontProc extends MultipleNamedProcedure2 {
        public LoadFontProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply2(Object arg1,Object arg2) throws Throwable {
            String filePath = SchemeUtils.anyToString( arg1 );
            float  fontSize = SchemeUtils.toFloat( arg2 );
            Font font = Kawapad.loadFont( filePath, fontSize );
            Kawapad kawapad = Kawapad.getCurrent();
            kawapad.setFont( font );
            return Values.empty;
        }
    }


    public static final LamuDocument loadFontUIDoc = new LamuDocument(){{
        setNames( "load-font-ui" );
        setParameterDescription( "" );
        addParameter( 0, "file-size", "string", null , false, "Specifies the path to the font file. " );
        addParameter( 0, "font-size", "number", null , false, "Specifies its font size. " );
        setReturnValueDescription( "::void" );
        setShortDescription( "Set the main font of the ui." );
        setLongDescription( ""
            + "_<name/>_ loads a file from the specified file and "
            + "set it as the default font of the current ui. "
            + "" );
    }};

    public static final MultipleNamedProcedure2 loadFontUIProc = new LoadFontUIProc(new String[] { "load-font-ui" });
    public static final class LoadFontUIProc extends MultipleNamedProcedure2 {
        public LoadFontUIProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply2(Object arg1,Object arg2) throws Throwable {
            String filePath = SchemeUtils.anyToString( arg1 );
            float  fontSize = SchemeUtils.toFloat( arg2 );
            Font font = Kawapad.loadFont( filePath, fontSize );
            Kawapad.setUIFont( new FontUIResource(font));
            return Values.empty;
        }
    }

    public static final MultipleNamedProcedureN addLispKeywordProc = new AddLispKeyword(new String[] { "add-lisp-keyword" });
    public static final class AddLispKeyword extends MultipleNamedProcedureN {
        public AddLispKeyword(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Kawapad.getCurrent().addAllLispKeywords( SchemeUtils.schemeStringListToJavaStringList( Arrays.asList( args )));
            return Values.empty;
        }
    }

    public static final MultipleNamedProcedureN deleteLispKeywordProc = new DeleteLispKeywordProc(new String[] { "delete-lisp-keyword" });
    public static final class DeleteLispKeywordProc extends MultipleNamedProcedureN {
        public DeleteLispKeywordProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Kawapad.getCurrent().deleteAllLispKeywords( SchemeUtils.schemeStringListToJavaStringList( Arrays.asList( args )));
            return Values.empty;
        }
    }

    public static final MultipleNamedProcedureN addSyntaxKeywordProc = new AddSyntaxKeywordProc(new String[] { "add-syntax-keyword" });
    public static final class AddSyntaxKeywordProc extends MultipleNamedProcedureN {
        public AddSyntaxKeywordProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Kawapad.getCurrent().addAllLispKeywords( SchemeUtils.schemeStringListToJavaStringList( Arrays.asList( args )));
            return Values.empty;
        }
    }

    public static final MultipleNamedProcedureN deleteSyntaxKeywordProc = new DeleteSyntaxKeywordProc(new String[] { "delete-syntax-keyword" });
    public static final class DeleteSyntaxKeywordProc extends MultipleNamedProcedureN {
        public DeleteSyntaxKeywordProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            Kawapad.getCurrent().deleteAllLispKeywords( SchemeUtils.schemeStringListToJavaStringList( Arrays.asList( args )));
            return Values.empty;
        }
    }



    public static final MultipleNamedProcedure1 getSyntaxKeywordProc = new GetSyntaxKeywordProc(new String[] { "get-syntax-keywords" });
    public static final class GetSyntaxKeywordProc extends MultipleNamedProcedure1 {
        public GetSyntaxKeywordProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg1) throws Throwable {
            return LList.makeList( SchemeUtils.javaStringListToSchemeSymbolList( Kawapad.getCurrent().getLispKeywordList() ) );
        }
    }

    public static final MultipleNamedProcedure0 pwdProc = new PwdProc(new String[] { "pwd" });
    public static final class PwdProc extends MultipleNamedProcedure0 {
        public PwdProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return SchemeUtils.toSchemeString( Kawapad.getCurrent().getCurrentDirectory().toString() );
        }
    }

    public static final MultipleNamedProcedureN setSyntaxColorProc = new SetSyntaxColorProc(new String[] { "set-syntax-color" });
    public static final class SetSyntaxColorProc extends MultipleNamedProcedureN {
        public SetSyntaxColorProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply2(Object arg1, Object arg2) throws Throwable {
            SyntaxElement syntaxElement = Kawapad.getCurrent().getSyntaxHighlighter().getSyntaxElementList().get(
                KawapadSyntaxElementType.schemeValueOf((Symbol)arg1));
            syntaxElement.setForegroundColor((Color)arg2);
            return Values.empty; 
        }

        @Override
        public Object apply3(Object arg1, Object arg2, Object arg3) throws Throwable {
            SyntaxElement syntaxElement = Kawapad.getCurrent().getSyntaxHighlighter().getSyntaxElementList().get(
                KawapadSyntaxElementType.schemeValueOf((Symbol)arg1));
            syntaxElement.setForegroundColor((Color)arg2);
            syntaxElement.setBackgroundColor((Color)arg3);
            return Values.empty; 
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            WrongArguments.checkArgCount( this.getName() , 2, 3, args.length );
            if ( args.length == 2 )
                return apply2( args[0], args[1] );
            else if ( args.length == 3 )
                return apply3( args[0], args[1], args[2] );

            throw new InternalError();
        }
    }

    public static final CurrentFileProc currentFileProc = new CurrentFileProc(new String[] { "current-file" });
    public static final class CurrentFileProc extends MultipleNamedProcedure0 {
        public CurrentFileProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            return SchemeUtils.toSchemeString( Kawapad.getCurrent().getCurrentFile().toString() );
        }
    }

    @Override
    public void run() {
        initSchemeProc( Environment.getCurrent() );
    }

    public static void initSchemeProc( Environment env) {
        if ( ! SchemeUtils.isDefined(env, FLAG_DONE_INIT_PULSAR_SCRATCHPAD ) ) {
            SchemeUtils.defineVar(env, true, FLAG_DONE_INIT_PULSAR_SCRATCHPAD );  


            SchemeUtils.defineVar(env, new Kawapad.ConsoleObject(), "console" );


            SchemeUtils.defineLambda(env, kawapadProc );
            SchemeUtils.defineLambda(env, isKawapadPresentProc);

            SchemeUtils.defineLambda(env, kawapadFrameProc);
            SchemeUtils.defineLambda(env, isKawapadFramePresentProc );
            SchemeUtils.defineLambda(env, frameProc);

            SchemeUtils.defineLambda(env, isFramePresentProc);
            SchemeUtils.defineLambda(env, registerEventHandlerProc);
            SchemeUtils.defineLambda(env, unregisterEventHandlerProc );
            SchemeUtils.defineLambda(env, prettifyProc );

            //            // deprecated?
            //            SchemeUtils.defineVar(env, new PulsarProcedure1() {
            //                @Override
            //                public Object apply1(Object arg1 ) throws Throwable {
            //                    return Kawapad.correctIndentation( getCurrent(), SchemeUtils.anyToString(arg1));
            //                }
            //            }, "prettify" );

            KawapadTextualIncrement.initScheme( env );

            SchemeUtils.defineLambda(env, loadFontProc );
            SchemeUtils.defineLambda(env, loadFontUIProc );
            SchemeUtils.defineLambda(env, addLispKeywordProc );
            SchemeUtils.defineLambda(env, deleteLispKeywordProc );
            SchemeUtils.defineLambda(env, addSyntaxKeywordProc );
            SchemeUtils.defineLambda(env, deleteSyntaxKeywordProc );
            SchemeUtils.defineLambda(env, getSyntaxKeywordProc );
            SchemeUtils.defineLambda(env, setSyntaxColorProc);
            SchemeUtils.defineLambda(env, pwdProc );
            SchemeUtils.defineLambda(env, currentFileProc );

        }
    }

    public static void initScheme( Scheme scheme ) {
        logInfo( "Kawapad#staticInitScheme" );
        Environment env = scheme.getEnvironment();
        initSchemeProc(env);
        SchemeEvaluatorUtils.executeExternalFile( scheme, null, "kawapad user extension", Kawapad.getExtFile() );
    }

}
