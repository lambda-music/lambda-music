package kawapad;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.regex.Pattern;

import javax.swing.text.AttributeSet;

import gnu.mapping.Symbol;
import pulsar.lib.scheme.SchemeUtils;
import pulsar.lib.scheme.scretary.SchemeSecretary;

class KawapadSyntaxHighlighter extends SyntaxHighlighter {
    private static final boolean DEBUG=true;
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
        Kawapad.logInfo( "createKeywordPattern()" );
        ElapsedTime ep = new ElapsedTime();
        if (DEBUG) ep.start();
        synchronized ( Kawapad.class ) {
            // vvv IS THIS REALLY NECESSARY?????? TODO (Wed, 11 Sep 2019 03:01:16 +0900)  vvvv
//            List<String> keywordList = new ArrayList<>();
//          keywordList.addAll( SchemeUtils.getAllKey( kawapad.getSchemeSecretary() ) );
            List<String> keywordList = SchemeUtils.getAllKey( kawapad.getSchemeSecretary() );
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            keywordList.addAll( kawapad.lispKeywordList );
            keywordList.sort( Kawapad.KEYWORD_COMPARATOR );
            for ( ListIterator<String> i=keywordList.listIterator(); i.hasNext(); ) {
                String s = i.next();
                i.set( Pattern.quote( s ) );
            }
            if (DEBUG) ep.end();
            if (DEBUG) logInfo( ep.getMessage( "createKeywordPattern(list up)" ) );
            if (DEBUG) ep.start();
            Pattern p = Pattern.compile(
                REGEX_NON_WORD_L + "(?<"+GROUP+">" + String.join( "|",  keywordList ) + ")" + REGEX_NON_WORD_R );
            if (DEBUG) ep.end();
            if (DEBUG) logInfo( ep.getMessage( "createKeywordPattern(compile)" ) );
            return p;
        }
    }

    boolean enabledPunctuation = false;
    boolean enabledKeyword = false;
    public boolean isEnabledPunctuation() {
        return enabledPunctuation;
    }
    public void setEnabledPunctuation(boolean enabledPunctuation) {
        this.enabledPunctuation = enabledPunctuation;
    }
    public boolean isEnabledKeyword() {
        return enabledKeyword;
    }
    public void setEnabledKeyword(boolean enabledKeyword) {
        this.enabledKeyword = enabledKeyword;
    }

    protected Collection<SyntaxElement> createSyntaxElementList() {
        ArrayList<SyntaxElement> list = new ArrayList<>(5);
        if ( enabledPunctuation )
            list.add( SyntaxHighlighter.createSyntaxElement(
                KawapadSyntaxElementType.PUNCTUATION,
                Pattern.compile( "(?<K>(?:\\(|\\)|\\:|\\'|\\#))" ),
                defaultPunctuationColor ));
        if ( enabledKeyword )
            list.add( SyntaxHighlighter.createSyntaxElement(
                KawapadSyntaxElementType.KEYWORD,
                createKeywordPattern(), 
                defaultKeywordColor )); 
        list.add( SyntaxHighlighter.createSyntaxElement(
            KawapadSyntaxElementType.STRING,
            Pattern.compile( "\\\"[\\s\\S]*?\\\"", Pattern.MULTILINE ),
            defaultStringColor )); 
        list.add( SyntaxHighlighter.createSyntaxElement(
            KawapadSyntaxElementType.BLOCK_COMMENT,
            Pattern.compile( "\\#\\|[\\s\\S]*?\\|\\#", Pattern.MULTILINE ),
            defaultBlockCommentColor )); 
        list.add( SyntaxHighlighter.createSyntaxElement(
            KawapadSyntaxElementType.LINE_COMMENT,
            Pattern.compile( ";.*$" , Pattern.MULTILINE ),
            defaultLineCommentColor ));
        return list;
    }
    @Override
    public AttributeSet getDefaultAttributeSet() {
        return SyntaxHighlighter.createAttributeSet( kawapad.getForeground() );
    }
}
