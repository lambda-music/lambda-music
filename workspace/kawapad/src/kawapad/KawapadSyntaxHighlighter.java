package kawapad;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.regex.Pattern;

import javax.swing.text.AttributeSet;

import gnu.lists.LList;
import gnu.mapping.Symbol;
import lamu.lib.scheme.SchemeEngine;
import lamu.lib.scheme.SchemeResult;
import lamu.lib.scheme.SchemeUtils;

class KawapadSyntaxHighlighter extends SyntaxHighlighter {
    private static final boolean DEBUG=false;
    public static enum KawapadSyntaxElementType {
        NONE,
        PUNCTUATION,
        VARIABLE,
        HIGHLIGHT,
        NUMBER,
        SYMBOL,
        KEYWORD,
        HASH,
        SHEBANG,
        STRING,
        LINE_COMMENT,
        BLOCK_COMMENT;
        static KawapadSyntaxHighlighter.KawapadSyntaxElementType schemeValueOf( Symbol symbol ) {
            String str = SchemeUtils.schemeSymbolToJavaString( symbol ).toUpperCase().replaceAll( "-" , "_" );
            return valueOf( str );
        }
    }
    private static final String REGEX_NON_WORD_L = "(?<=[^'a-zA-Z0-9-_])";
    private static final String REGEX_NON_WORD_R = "(?=[^:'a-zA-Z0-9-_])";
    final AttributeSet defaultShebangColor       = SyntaxHighlighter.createAttributeSet( new Color( 0x00,0x80,0x00,0xff ) );
    final AttributeSet defaultBlockCommentColor  = SyntaxHighlighter.createAttributeSet( new Color( 0xa0,0xa0,0xa0,0xff ) );
    final AttributeSet defaultLineCommentColor   = SyntaxHighlighter.createAttributeSet( new Color( 0xa0,0xa0,0xa0,0xff ) );
    final AttributeSet defaultHighlightColor     = SyntaxHighlighter.createAttributeSet( new Color( 0x80,0x80,0x00,0xff ), new Color( 0x00,0xff,0xff,0xff ) );
    final AttributeSet defaultStringColor        = SyntaxHighlighter.createAttributeSet( new Color( 0x80,0x80,0x00,0xff ) );
    final AttributeSet defaultNumberColor        = SyntaxHighlighter.createAttributeSet( new Color( 0xff,0x80,0x00,0xff ) );
    final AttributeSet defaultSymbolColor        = SyntaxHighlighter.createAttributeSet( new Color( 0x80,0x80,0x00,0xff ) );
    final AttributeSet defaultKeywordColor       = SyntaxHighlighter.createAttributeSet( new Color( 0x00,0x80,0x40,0xff ));
    final AttributeSet defaultHashColor          = SyntaxHighlighter.createAttributeSet( new Color( 0xff,0x40,0x40,0xff ));
    final AttributeSet defaultPunctuationColor   = SyntaxHighlighter.createAttributeSet( new Color( 0x50,0x50,0x50,0xff ));
    final AttributeSet defaultVariableColor      = SyntaxHighlighter.createAttributeSet( new Color( 0x00,0x00,0xff,0xff ));
    SchemeEngine schemeEngine;
    Kawapad kawapad;
    KawapadSyntaxHighlighter(Kawapad kawapad) {
        super( kawapad );
        this.kawapad = kawapad;
    }
    
    /**
     * CAUTION : THIS METHOD IS NOT TESTED SINCE THIS METHOD WAS MODIFED TO USE evaluate() METHOD. 
     * @return
     */
    private Pattern createKeywordPattern() {
        Kawapad.logInfo( "createKeywordPattern()" );
        ElapsedTime ep = new ElapsedTime();
        if (DEBUG) ep.start();

        synchronized ( Kawapad.class ) {
            // vvv IS THIS REALLY NECESSARY?????? TODO (Wed, 11 Sep 2019 03:01:16 +0900)  vvvv
//            List<String> keywordList = new ArrayList<>();
//          keywordList.addAll( SchemeUtils.getAllKey( kawapad.getSchemeSecretary() ) );

            SchemeResult result = kawapad.getSchemeEngine().getSchemeEvaluator().evaluate( 
                "(environment-fold (interaction-environment) cons '())", "get-all" );
            
            result.throwIfError();
            
            List<String> keywordList =
                    new ArrayList<> (
                            SchemeUtils.convertList( 
                                new ArrayList<>( (LList)result.getValue() ),
                                (v)->SchemeUtils.anyToString( v ) ) );
            
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

    boolean enabledPunctuation = true;
    boolean enabledKeyword = true;
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
                KawapadSyntaxElementType.VARIABLE,
                createKeywordPattern(), 
                defaultVariableColor )); 
        list.add( SyntaxHighlighter.createSyntaxElement(
            KawapadSyntaxElementType.HIGHLIGHT,
            null,
            defaultHighlightColor )); 
        list.add( SyntaxHighlighter.createSyntaxElement(
            KawapadSyntaxElementType.NUMBER,
            Pattern.compile( REGEX_NON_WORD_L + "(?<K>[0-9]+((/|\\.)[0-9]+)?)" + REGEX_NON_WORD_R ),
            defaultNumberColor )); 
        list.add( SyntaxHighlighter.createSyntaxElement(
            KawapadSyntaxElementType.SYMBOL,
            Pattern.compile( "(?<K>'[a-zA-Z-_]+)"  ),
            defaultSymbolColor )); 
        list.add( SyntaxHighlighter.createSyntaxElement(
            KawapadSyntaxElementType.KEYWORD,
            Pattern.compile( "(?<K>[a-zA-Z-_]+:)" ),
            defaultKeywordColor )); 
        list.add( SyntaxHighlighter.createSyntaxElement(
            KawapadSyntaxElementType.HASH,
            Pattern.compile( "(?<K>\\#[a-zA-Z-_]+)" ),
            defaultHashColor )); 
        list.add( SyntaxHighlighter.createSyntaxElement(
            KawapadSyntaxElementType.SHEBANG,
            Pattern.compile( "^\\#\\!.*\\R" ),
            defaultShebangColor )); 
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
    
    /**
     * this method may not be necessary anymore.
     * (Fri, 27 Mar 2020 04:36:53 +0900)
     */
    @Override
    public SyntaxElement getDefaultAttributeSet() {
        AttributeSet attr =  SyntaxHighlighter.createAttributeSet( kawapad.getForeground() );
        SyntaxElement element = SyntaxHighlighter.createSyntaxElement( KawapadSyntaxElementType.NONE, null, attr );
        SyntaxHighlighter.setSyntaxElement( attr, element ); 
        return element;
    }
}
