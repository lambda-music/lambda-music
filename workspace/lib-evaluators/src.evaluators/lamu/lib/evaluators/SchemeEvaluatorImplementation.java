package lamu.lib.evaluators;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.util.logging.Level;
import java.util.logging.Logger;

// MODIFIED ON (Fri, 24 Apr 2020 14:46:28 +0900) >>>
//import lamu.lib.log.Logger;
// MODIFIED ON (Fri, 24 Apr 2020 14:46:28 +0900) <<<
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.lists.Consumer;
import gnu.mapping.CallContext;
import kawa.Shell;
import kawa.standard.Scheme;

class SchemeEvaluatorImplementation {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
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

    static String readAll( Reader r ) {
        StringBuilder sb = new StringBuilder();
        char[] cb = new char[ 4096 ];
        int s;
        try {
            while( 0 < ( s = r.read( cb ) ) ){
                sb.append( cb, 0, s );
            }
            return sb.toString();
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            try {
                r.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    static final ThreadLocal<File> currentBaseFile = new ThreadLocal<File>();

    static SchemeResult evaluateSchemeProc( 
        Scheme scheme, Runnable threadInitializer, 
        Reader scriptReader, File currentFile, String schemeScriptURI )
    {
        String script;
        Reader bufferedScriptReader;
        try {
            script = readAll(scriptReader);
            bufferedScriptReader = new StringReader( script );
        } finally {
            try {
                scriptReader.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        
        logInfo( "=== Execute Scheme ===\n" + script + "\n==============\n");
        try {
            if ( scheme == null ) {
                return SchemeResult.createError( 
                    new NullPointerException( 
                        "could not execute the given script because Scheme engine is not initialized. \n" + 
                            script ));
            }

            // schemeSecretary.initializeSchemeForCurrentThread();
            // synchronized ( scheme ) // disabled   
            {
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

                Path savedLoadPath = (Path) Shell.currentLoadPath.get();
                File savedBaseFile = currentBaseFile.get(); 
                try {
                    // Initialize `currentLoadPath`.
                    {
                        File parentDirectory;
                        if ( currentFile != null ) {
                            parentDirectory = currentFile.getAbsoluteFile().getParentFile();
                        } else {
                            parentDirectory = new File(".").getAbsoluteFile().getCanonicalFile();
                        }

                        Shell.currentLoadPath.set( Path.valueOf( parentDirectory ) );
                    }
                    
                    // Initialize `currentBaseFile`.
                    {
                        currentBaseFile.set( currentFile == null ? null : currentFile.getAbsoluteFile() );
                    }

                    // // I feel overriding "load" by "load-relative" is too risky. It
                    // // may destroy the compatibility inside the kawa library; we
                    // // decide to call it "source".  Usually ,this kind of
                    // // initialization process should be done in staticInitScheme()
                    // // method.  But we want to make it visible here that "source"
                    // // is available in this way.  (Mon, 09 Sep 2019 04:31:19 +0900)
                    // SchemeUtils.defineVar(env, load.loadRelative , "source" );
                    // Moved to SchemeSecretary (Thu, 19 Dec 2019 02:43:01 +0900)
                    //
                    // Note : Now the application should use source
                    //
                    // Note : (Mon, 13 Apr 2020 11:58:27 +0900)
                    //        See lamu.lib.scheme.SchemeEvaluatorLib.initScheme(Environment)


                    CallContext ctx = CallContext.getInstance();
                    Consumer out = Shell.getOutputConsumer(OutPort.outDefault());
                    if (out != null) {
                        ctx.consumer = out;
                    }

                    // {@link kawa.Shell#runFile(InputStream, Path, gnu.mapping.Environment, boolean, int) }
                    Object resultValue = scheme.eval( new InPort( bufferedScriptReader, Path.valueOf( schemeScriptURI ) ) );
                    // Object result = Shell.run( schemeScript, schemeScriptURI, scheme.getEnvironment(), true, 0 ); 

                    return SchemeResult.createSucceededByObject( resultValue );
                } catch (Throwable e) {
                    logError( "** Execute Scheme *** error ", e ); 

                    return SchemeResult.createError( e );
                } finally {
                    try {
                        Shell.currentLoadPath.set( savedLoadPath );
                    } catch ( Throwable e ) {
                        logError("ignored",e);
                    }
                    try {
                        currentBaseFile.set( savedBaseFile);
                    } catch ( Throwable e ) {
                        logError("ignored",e);
                    }
                }
            }
        } finally {
            try {
                bufferedScriptReader.close();
            } catch (Throwable e1) {
                logError( "failed to close the stream" , e1 );
            }
        }
    }
}
