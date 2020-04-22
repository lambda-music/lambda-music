package lamu.lib.helps;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;

public class LamuAbstractDocument {
    /**
     * 
     * This outputs the formatted documentation 
     * @param outputFile
     *     the file name to be output.
     *     
     * @throws IOException
     */
    public static void outputAvailableReferences( String outputFile ) throws IOException {
        String output = 
            String.join( ",", LamuDocument.getAllAvailableCategory() );
        
        if ( outputFile == null /* || "-".equals( outputFile ) */ ) {
            System.out.println( output );      
        } else {
            FileOutputStream fo = null;
            try {
                fo = new FileOutputStream( new File( outputFile ) );
                fo.write( output.getBytes( Charset.forName( "utf-8" ) ) );
                System.err.println( output );      
                fo.flush();
            } finally {
                if ( fo != null )
                    fo.close();
            }
        }
    }
    


    public static void outputReference( String categoryName, String outputFile ) throws FileNotFoundException, IOException {
        String str =  MarkdownDocumentFormatter.createMarkdownHelp( 
            LamuDocument.get( 
                LamuDocumentCondition.createConditionByCategory( categoryName )));
        
        if ( outputFile == null /* || "-".equals( outputFile ) */ ) {
            System.out.println( str );      
        } else {
            FileOutputStream fo = null;
            try {
                fo = new FileOutputStream( new File( outputFile ) );
                fo.write( str.getBytes( Charset.forName( "utf-8" ) ) );
                System.err.println( str );      
                fo.flush();
            } finally {
                if ( fo != null )
                    fo.close();
            }
        }
    }
}
