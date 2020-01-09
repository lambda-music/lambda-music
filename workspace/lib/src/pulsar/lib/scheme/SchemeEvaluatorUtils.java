package pulsar.lib.scheme;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;

import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.lists.Consumer;
import gnu.mapping.CallContext;
import kawa.Shell;
import kawa.standard.Scheme;
import pulsar.lib.log.PulsarLogger;

class SchemeEvaluatorUtils {
    static final PulsarLogger LOGGER = PulsarLogger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    static final boolean DEBUG = false;
    
//    public static void execSchemeFromResource( Scheme scheme, Class parentClass, String resourcePath ) throws IOException {
//        SchemeExecutorUtils.evaluateScheme( 
//            scheme, null, 
//            new InputStreamReader( parentClass.getResource( resourcePath ).openStream() ), 
//            null, null, resourcePath 
//            ).throwIfError();
//    }

//    public static SchemeResult evaluateScheme( 
//            Scheme scheme, Runnable threadInitializer, 
//            Reader schemeScript, File currentDirectory, File currentFile, String schemeScriptURI )
//    {
//        return evaluateSchemeProc( scheme, threadInitializer, schemeScript, currentDirectory, currentFile, schemeScriptURI );
//    }

    static SchemeResult evaluateSchemeProc( 
            Scheme scheme, Runnable threadInitializer, 
            Reader schemeScript, File currentDirectory, File currentFile, String schemeScriptURI )
    {
        logInfo( "=== Execute Scheme ===" );

        // schemeSecretary.initializeSchemeForCurrentThread();
        synchronized ( scheme ) {
            // Initialize threads : 
            // Therefore threadInitializer should keep the default thread initializer at here,
            // calling initializeCurrentThread() here is merely a fallback for initializing Scheme object.
            // Call initializeCurrentThread() here in case that no thread initializer was specified.
            
            SchemeEvaluator.initializeCurrentThread( scheme );
            if ( threadInitializer == null ) {
                logWarn( "No thread initializer was specified " );
            } else {
                if ( DEBUG )
                    logInfo( "threadInitializer=" + threadInitializer.toString() );
                try {
                    threadInitializer.run();
                } catch ( Throwable t ) {
                    logError( "The thread initializer specified failed. ", t);
                }
            }
            
            
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
    
                // // I feel overriding "load" by "load-relative" is too risky. It
                // // may destroy the compatibility inside the kawa library; we
                // // decide to call it "source".  Usually ,this kind of
                // // initialization process should be done in staticInitScheme()
                // // method.  But we want to make it visible here that "source"
                // // is available in this way.  (Mon, 09 Sep 2019 04:31:19 +0900)
                // SchemeUtils.defineVar(env, load.loadRelative , "source" );
                // Moved to SchemeSecretary (Thu, 19 Dec 2019 02:43:01 +0900)
                
    
                CallContext ctx = CallContext.getInstance();
                Consumer out = Shell.getOutputConsumer(OutPort.outDefault());
                if (out != null) {
                    ctx.consumer = out;
                }
                
                 // {@link kawa.Shell#runFile(InputStream, Path, gnu.mapping.Environment, boolean, int) }
                Object resultValue = scheme.eval( new InPort( schemeScript, Path.valueOf( schemeScriptURI ) ) );
                // Object result = Shell.run( schemeScript, schemeScriptURI, scheme.getEnvironment(), true, 0 ); 
    
                return SchemeResult.createSucceededByObject( resultValue );
            } catch (Throwable e) {
                logError( "** Execute Scheme *** error ", e ); 

                return SchemeResult.createError( e );
            } finally {
                try {
                    schemeScript.close();
                } catch (IOException e1) {
                    SchemeUtils.logError( "failed to close the stream" , e1 );
                }
                
                Shell.currentLoadPath.set( savedPath );
            }
        }
    }
}
