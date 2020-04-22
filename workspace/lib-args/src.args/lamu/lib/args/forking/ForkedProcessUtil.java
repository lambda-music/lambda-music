package lamu.lib.args.forking;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.invoke.MethodHandles;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;

import lamu.lib.log.Logger;

public class ForkedProcessUtil {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }

    static String readOutput( InputStream i ) throws IOException{
        BufferedReader reader = 
                new BufferedReader(new InputStreamReader(i));
        StringBuilder builder = new StringBuilder();
        String line = null;
        while ( (line = reader.readLine()) != null) {
            builder.append(line);
            builder.append(System.getProperty("line.separator"));
        }
        String result = builder.toString();
        return result;
    }

    static Process executeProcess( List<String> arguments ) throws IOException{
        ArrayList<String> fullArguments = new ArrayList<>();
        fullArguments.addAll( arguments );
        logInfo( String.join( " " , fullArguments ) );
        return new ProcessBuilder( fullArguments ).start();
    }

    static Process executeJavaProcess( String canonicalNameOfMainClass, List<String> arguments ) throws IOException{
        ArrayList<String> fullArguments = new ArrayList<>();
        fullArguments.addAll( getJavaArguments( canonicalNameOfMainClass ) );
        fullArguments.addAll( arguments );
        logInfo( String.join( " " , fullArguments ) );
        ProcessBuilder b = new ProcessBuilder( fullArguments );
//        b.inheritIO();
        return b.start();
    }
    
    public static List<String> getJavaArguments( String canonicalNameOfMainClass ) {
        // https://stackoverflow.com/questions/1490869/how-to-get-vm-arguments-from-inside-of-java-application
        ArrayList<String> fullArguments = new ArrayList<>();
        RuntimeMXBean r = ManagementFactory.getRuntimeMXBean();
        fullArguments.add( "java" );
        fullArguments.addAll( argFilter( r.getInputArguments()) );
        fullArguments.add("-classpath");
        fullArguments.add( r.getClassPath() );
        fullArguments.add( canonicalNameOfMainClass );
        return fullArguments;
    }
    private static Collection<? extends String> argFilter(List<String> args) {
        ArrayList<String> result = new ArrayList<>();
        
        for ( String arg : args ) {
            if ( arg.startsWith("-agentlib:jdwp=") ) {
                continue;
            }
            result.add( arg );
        }
        
        return result;
    }
}
