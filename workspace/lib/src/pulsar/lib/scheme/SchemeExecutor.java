package pulsar.lib.scheme;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Collection;
import java.util.Map;

import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.lists.Consumer;
import gnu.mapping.CallContext;
import gnu.mapping.Environment;
import kawa.Shell;
import kawa.standard.Scheme;
import kawa.standard.load;
import pulsar.lib.scheme.scretary.SchemeSecretary;

public class SchemeExecutor {
    public static void execSchemeFromResource( Scheme scheme, Class parentClass, String resourcePath ) throws IOException {
        SchemeExecutor.evaluateScheme( 
            scheme, 
            null, 
            null, 
            new InputStreamReader( parentClass.getResource( resourcePath ).openStream() ), 
            null, null, resourcePath 
            ).throwIfError();
    }

    public static SchemeResult evaluateScheme( 
            Scheme scheme, Collection<Runnable> threadInitializers, 
            Map<String, Object> variables, Reader schemeScript, 
            File currentDirectory, File currentFile, String schemeScriptURI )
    {
        //              schemeSecretary.initializeSchemeForCurrentThread();
        SchemeSecretary.initializeCurrentThread( scheme );
        synchronized ( scheme ) {
            Environment env = scheme.getEnvironment();
            
            if ( threadInitializers != null )
                initializeThread( threadInitializers );
            if ( variables != null )
                initializeVariables( env, variables );
            
            SchemeUtils.putVar( env , "scheme", scheme );
            
            // Set current directory to the default load path.
            // Note that <i>Shell.currentLoadPath</i> is not documented in the official documentation.
            // The variable Shell.currentLoadPath is only referred in kawa.standard.load and 
            // this is the only way to affect the //load//'s behavior.
            
            // FIXME
            // Now I realized that currentLoadPath only affect to (load-relative) and
            // it will by no means affect to (load) sigh. 
            // This code effectively makes (load-relative) current directory aware.
            // But I think it is cumbersome to ask users to use load-relative procedure in
            // every situation. IMO load-relative supposed to be default.
            // I'm thinking about it.  (Thu, 15 Aug 2019 16:21:22 +0900)
            
            
            Path savedPath = (Path) Shell.currentLoadPath.get();
            try {
                File parentDirectory;
                if ( currentFile != null ) {
                    parentDirectory = currentFile.getParentFile();
                } else {
                    parentDirectory = new File(".").getAbsoluteFile().getCanonicalFile();
                }
    
                Shell.currentLoadPath.set( Path.valueOf( parentDirectory ) );
    
                // I feel overriding "load" by "load-relative" is too risky. It
                // may destroy the compatibility inside the kawa library; we
                // decide to call it "source".  Usually ,this kind of
                // initialization process should be done in staticInitScheme()
                // method.  But we want to make it visible here that "source"
                // is available in this way.  (Mon, 09 Sep 2019 04:31:19 +0900)
                SchemeUtils.defineVar(env, load.loadRelative , "source" );
    
    
                CallContext ctx = CallContext.getInstance();
                Consumer out = Shell.getOutputConsumer(OutPort.outDefault());
                if (out != null) {
                    ctx.consumer = out;
                }
                
                 // {@link kawa.Shell#runFile(InputStream, Path, gnu.mapping.Environment, boolean, int) }
                Object resultValue = scheme.eval( new InPort( schemeScript, Path.valueOf( schemeScriptURI ) ) );
                // Object result = Shell.run( schemeScript, schemeScriptURI, scheme.getEnvironment(), true, 0 ); 
    
                if ( resultValue == null ) {
                    return SchemeResult.createNull();
                } else {
                    if ( Descriptive.isSchemeDocument( resultValue ) ) {
                        Object doc = Descriptive.getSchemeDocument(resultValue);
                        return SchemeResult.createSucceeded( true, doc, SchemePrinter.printDocument(doc) );
                    } else {
                        return SchemeResult.createSucceeded( false, resultValue, SchemePrinter.printSchemeValue(resultValue)  );
                    }
                }
            } catch (Throwable e) {
                return SchemeResult.createError( e );
            } finally {
                SchemeUtils.putVar( env , "scheme", false );
                
                if ( variables != null )
                    for ( Map.Entry<String, Object> e : variables.entrySet() )
                        SchemeUtils.putVar( env , e.getKey(), false );
                
                try {
                    schemeScript.close();
                } catch (IOException e1) {
                    SchemeUtils.logError( "failed to close the stream" , e1 );
                }
                
                Shell.currentLoadPath.set( savedPath );
            }
        }
    }


    public static void initializeThread( Collection<Runnable> threadInitializers ) {
        if ( threadInitializers != null )
            for ( Runnable r : threadInitializers ) {
                try {
                    r.run();
                } catch ( Throwable t ) {
                    SchemeUtils.logError( "", t );
                }
            }
    }

    public static void initializeVariables(Environment env, Map<String, Object> variables) {
        if ( variables != null )
            for ( Map.Entry<String, Object> e : variables.entrySet() )
                SchemeUtils.putVar( env , e.getKey(), e.getValue() );
    }

    public static void finalizeVariables(Environment env, Map<String, Object> variables) {
        if ( variables != null )
            for ( Map.Entry<String, Object> e : variables.entrySet() )
                SchemeUtils.putVar( env , e.getKey(), false );
    }

    ////////////////////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////////////////////

    public static String endWithLineFeed(String s) {
        if ( s == null )
            return null;
        else if ( s.equals( "" ) )
            return "";
        else if ( s.endsWith("\n" ) )
            return s;
        else
            return s + "\n"; 
    }

    public static String formatResult( String s ) {
        if ( s == null )
            return null;
        else if ( s.equals( "" ) )
            return "";
        else
            return "#|\n" + endWithLineFeed( s ) + "|#\n";
    }

    
    
}
