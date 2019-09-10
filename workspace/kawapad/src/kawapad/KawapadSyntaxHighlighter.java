package kawapad;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.regex.Pattern;

import javax.swing.text.AttributeSet;

import gnu.mapping.Symbol;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.scheme.scretary.SchemeSecretary;

class KawapadSyntaxHighlighter extends SyntaxHighlighter {
    public static enum KawapadSyntaxElementType {
        KEYWORD,
        PUNCTUATION,
        STRING,
        LINE_COMMENT,
        BLOCK_COMMENT;
        static KawapadSyntaxHighlighter.KawapadSyntaxElementType schemeValueOf( Symbol symbol ) {
            String str = SchemeUtils.schemeSymbolToJavaString( symbol ).toUpperCase().replaceAll( "-" , "_" );
            return valueOf( str );
        }
    }
    private static final String REGEX_NON_WORD_L = "(?<=[^a-zA-Z0-9-_])";
    private static final String REGEX_NON_WORD_R = "(?=[^a-zA-Z0-9-_])";
    final AttributeSet defaultBlockCommentColor  = SyntaxHighlighter.grayAttributeSet;
    final AttributeSet defaultLineCommentColor   = SyntaxHighlighter.grayAttributeSet;
    final AttributeSet defaultStringColor        = SyntaxHighlighter.darkGreenAttributeSet;
    final AttributeSet defaultPunctuationColor   = SyntaxHighlighter.redAttributeSet;
    final AttributeSet defaultKeywordColor       = SyntaxHighlighter.orangeAttributeSet;
    SchemeSecretary schemeSecretary;
    Kawapad kawapad;
    KawapadSyntaxHighlighter(Kawapad kawapad) {
        super( kawapad );
        this.kawapad = kawapad;
    }
    private Pattern createKeywordPattern() {
        synchronized ( Kawapad.class ) {
            List<String> keywordList = new ArrayList<>();
            // vvv IS THIS REALLY NECESSARY?????? TODO (Wed, 11 Sep 2019 03:01:16 +0900)  vvvv
            keywordList.addAll( SchemeUtils.getAllKey( kawapad.getSchemeSecretary() ) );
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            keywordList.addAll( kawapad.lispKeywordList );
            keywordList.sort( Kawapad.KEYWORD_COMPARATOR );
            for ( ListIterator<String> i=keywordList.listIterator(); i.hasNext(); ) {
                String s = i.next();
                i.set( Pattern.quote( s ) );
            }
            return Pattern.compile( 
                REGEX_NON_WORD_L + "(?<"+GROUP+">" + String.join( "|",  keywordList ) + ")" + REGEX_NON_WORD_R );
        }
    }
    
    protected Collection<SyntaxElement> createSyntaxElementList() {
        if ( false ) {
            return Arrays.asList(
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.PUNCTUATION,
                    Pattern.compile( "(?<K>(?:\\(|\\)|\\:|\\'|\\#))" ),
                    defaultPunctuationColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.KEYWORD,
                    createKeywordPattern(), 
                    defaultKeywordColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.STRING,
                    Pattern.compile( "\\\"[\\s\\S]*?\\\"", Pattern.MULTILINE ),
                    defaultStringColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.BLOCK_COMMENT,
                    Pattern.compile( "\\#\\|[\\s\\S]*?\\|\\#", Pattern.MULTILINE ),
                    defaultBlockCommentColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.LINE_COMMENT,
                    Pattern.compile( ";.*$" ),
                    defaultLineCommentColor ) );
        }

        if ( true ) {
            return Arrays.asList(
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.KEYWORD,
                    createKeywordPattern(), 
                    defaultKeywordColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.STRING,
                    Pattern.compile( "\\\"[\\s\\S]*?\\\"", Pattern.MULTILINE ),
                    defaultStringColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.BLOCK_COMMENT,
                    Pattern.compile( "\\#\\|[\\s\\S]*?\\|\\#", Pattern.MULTILINE ),
                    defaultBlockCommentColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.LINE_COMMENT,
                    Pattern.compile( ";.*$", Pattern.MULTILINE  ),
                    defaultLineCommentColor ) );
        }
        if ( false ) {
            return Arrays.asList(
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.PUNCTUATION,
                    Pattern.compile( "(?<K>(?:\\(|\\)|\\:|\\'|\\#)+)" ),
                    defaultPunctuationColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.KEYWORD,
                    createKeywordPattern(), 
                    defaultKeywordColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.STRING,
                    Pattern.compile( "\\\"[\\s\\S]*?\\\"", Pattern.MULTILINE ),
                    defaultStringColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.BLOCK_COMMENT,
                    Pattern.compile( "\\#\\|[\\s\\S]*?\\|\\#", Pattern.MULTILINE ),
                    defaultBlockCommentColor ), 
                SyntaxHighlighter.createSyntaxElement(
                    KawapadSyntaxElementType.LINE_COMMENT,
                    Pattern.compile( ";.*$" ),
                    defaultLineCommentColor ) );
        }
        return null;
    }
    @Override
    public AttributeSet getDefaultAttributeSet() {
        return SyntaxHighlighter.createAttributeSet( kawapad.getForeground() );
    }
}