package lamu;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LamuQuotedStringSplitter {
    static final String TAG_END = "__RECALPER__";
    static final String TAG_BEGIN = "__REPLACER__";
    static final Pattern PAT_IN1 = Pattern.compile( "\"([^\"]*)\"" );
    static final Pattern PAT_IN2 = Pattern.compile( "(\\{[^\\{]*?\\})" );
    static final Pattern PAT_OUT = Pattern.compile( TAG_BEGIN + "([0-9]+)" + TAG_END );
    private static String createTag( int number ) {
        return TAG_BEGIN + String.format( "%05x", number  ) + TAG_END;
    }

    static List<String> splitString(String value) {
        List<String> substitution = new ArrayList<>();

        String substitutedValue = value;

        // Substitute every string which is surrounded by a pair of curly brackets.
        // Note that brackets are processed at first.
        {
            boolean found = true;
            while ( found ) {
                Matcher m = PAT_IN2.matcher( substitutedValue );
                StringBuffer sb = new StringBuffer();
                found = false;
                while (m.find()) {
                    int idx = substitution.size();  
                    substitution.add( m.group(1) );
                    m.appendReplacement(sb,  createTag(idx) );
                    found = true;
                }
                m.appendTail(sb);
                substitutedValue = sb.toString();
            }
        }

        // Substitute quotated strings.
        // Note that quotations are processed at second.
        {
            Matcher m = PAT_IN1.matcher( substitutedValue );
            StringBuffer sb = new StringBuffer();
            while (m.find()) {
                int idx = substitution.size();  
                substitution.add( m.group(1) );
                m.appendReplacement(sb,  createTag(idx) );
            }
            m.appendTail(sb);
            substitutedValue = sb.toString();
        }

        // The order 1. brackets 2. quotations  MATTERS!
        // This enables usage of pairs of quotations inside a set of brackets.


        //			System.out.println( substitution ); 

        List<String> resultList;
        {
            String[] split = substitutedValue.trim().split("[\\s]+");
            for ( int i=0; i<split.length; i++ ) {
                String stagedValue = split[i];

                boolean found = true;
                while (found) {
                    Matcher m = PAT_OUT.matcher( stagedValue );
                    StringBuffer sb = new StringBuffer();
                    found = false;
                    while (m.find()) {
                        found = true;
                        int idx = Integer.valueOf( m.group(1), 16);
                        String replacement = substitution.get(idx);
//                        System.err.println( replacement );
//                        System.err.println( replacement.replaceAll( "\\$", "\\\\\\$" ) );
                        m.appendReplacement( sb,  replacement.replaceAll("\\$", "\\\\\\$") );
                    }
                    m.appendTail(sb);
                    stagedValue = sb.toString();
                }
                split[i] = stagedValue;
            }
            resultList = Arrays.asList( split );
        }
        return resultList;
    }
    static class Test1 {
        public static String stringify( List<String> lst ) {
            StringBuffer sb = new StringBuffer();
            for ( int i=0; i<lst.size(); i++ ) {
                sb
                .append( '[' )
                .append(i)
                .append( ']')
                .append( " \"" )
                .append( lst.get(i) )
                .append( '"' )
                .append( "\n" );
            }
            return sb.toString();
        }
        static int counter =0;
        public static void output( List<String> lst ) {
            System.out.println( "=== Test" + (counter++) + " ===" );
            System.out.println( stringify( lst ) );
        }
        public static void main(String[] args) {

            output( splitString( "hello foo \"bar bum\" world" ) );
            output( splitString( "hello foo \"bar bum\" \"\" world" ) );
            output( splitString( "hello foo \"bar bum\" \"FOO BAR BUM\" world" ) );
            output( splitString( "\"FOO BAR BUM\"" ) );
            output( splitString( "\"FOO BAR BUM\" \"" ) );

            /*
             *  This method does not throw an error when it encounters to an unterminated quotation.
             *  This lacks perfection, but it works enough to do the job. Leave it untouched. 
             *  (Mon, 09 Mar 2020 16:09:06 +0900)  
             */
            output( splitString( "\"FOO BAR BUM\" \" sss" ) );


            output( splitString( "aaa {hello world} bbb" ) );
            output( splitString( "aaa $VAR{hello world} bbb" ) );

            /*
             * This function supports nested curly brackets.
             * 
             * [{ ss}, {hello __REPLACER__00000__RECALPER__ world}]
             * === Test8 ===
             * [0] "aaa"
             * [1] "$VAR{hello { ss} world}"
             * [2] "bbb"
             */
            output( splitString( "aaa $VAR{hello { ss} world} bbb" ) );
            output( splitString( "aaa $VAR{hello { FOO BAR } world } bbb" ) );


            /*
             * This function supports quotations inside a curly brackets.
             * 
             * [{ ss}, {hello __REPLACER__00000__RECALPER__ world}]
             * === Test8 ===
             * [0] "aaa"
             * [1] "$VAR{hello " FOO BAR " world }"
             * [2] "bbb"
             */
            output( splitString( "aaa $VAR{hello \" FOO BAR \" world } bbb" ) );
        }
    }
}
