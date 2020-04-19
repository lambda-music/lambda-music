package kawapad;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.ListIterator;
import java.util.regex.Pattern;

import javax.swing.text.AttributeSet;

import gnu.lists.LList;
import lamu.lib.scheme.SchemeResult;
import lamu.lib.scheme.SchemeUtils;

public class KawapadSyntaxHighlighter extends SyntaxHighlighter {
    private static final boolean DEBUG=false;
    private static final String REGEX_NON_WORD_L = "(?<=[^'a-zA-Z0-9-_])";
    private static final String REGEX_NON_WORD_R = "(?=[^:'a-zA-Z0-9-_])";
    
    public static AttributeSet DEFAULT_SHEBANG_COLOR        = SyntaxHighlighter.createAttributeSet( new Color( 0x00,0x80,0x00,0xff ) );
    public static AttributeSet DEFAULT_BLOCK_COMMENT_COLOR  = SyntaxHighlighter.createAttributeSet( new Color( 0xa0,0xa0,0xa0,0xff ) );
    public static AttributeSet DEFAULT_LINE_COMMENT_COLOR   = SyntaxHighlighter.createAttributeSet( new Color( 0xa0,0xa0,0xa0,0xff ) );
    public static AttributeSet DEFAULT_KEYWORD_HIGHLIGHT_COLOR      = SyntaxHighlighter.createAttributeSet( new Color( 0x80,0x80,0x00,0xff ), new Color( 0xc0,0xc0,0xff,0xff ) );
    public static AttributeSet DEFAULT_PARENTHESIS_HIGHLIGHT_COLOR  = SyntaxHighlighter.createAttributeSet( new Color( 0x80,0x80,0x00,0xff ), new Color( 0x00,0xc0,0xff,0xff ) );
    public static AttributeSet DEFAULT_STRING_COLOR        = SyntaxHighlighter.createAttributeSet( new Color( 0x80,0x80,0x00,0xff ) );
    public static AttributeSet DEFAULT_NUMBER_COLOR        = SyntaxHighlighter.createAttributeSet( new Color( 0xff,0x80,0x00,0xff ) );
    public static AttributeSet DEFAULT_SYMBOL_COLOR        = SyntaxHighlighter.createAttributeSet( new Color( 0x80,0x80,0x00,0xff ) );
    public static AttributeSet DEFAULT_KEYWORD_COLOR       = SyntaxHighlighter.createAttributeSet( new Color( 0x00,0x80,0x40,0xff ));
    public static AttributeSet DEFAULT_HASH_COLOR          = SyntaxHighlighter.createAttributeSet( new Color( 0xff,0x40,0x40,0xff ));
    public static AttributeSet DEFAULT_PUNCTUATION_COLOR   = SyntaxHighlighter.createAttributeSet( new Color( 0x50,0x50,0x50,0xff ));
    public static AttributeSet DEFAULT_VARIABLE_COLOR      = SyntaxHighlighter.createAttributeSet( new Color( 0x00,0x80,0xff,0xff ));
    
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

            SchemeResult result = kawapad.getEvaluator().evaluate( 
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

    @Override
    protected Collection<SyntaxElement> createSyntaxElementList() {
        ArrayList<SyntaxElement> list = new ArrayList<>(5);
        if ( enabledPunctuation )
            list.add( SyntaxElement.create(
                KawapadSyntaxElementType.PUNCTUATION,
                Pattern.compile( "(?<K>(?:\\(|\\)|\\:|\\'|\\#))" ),
                DEFAULT_PUNCTUATION_COLOR ));
        if ( enabledKeyword )
            list.add( SyntaxElement.create(
                KawapadSyntaxElementType.VARIABLE,
                createKeywordPattern(), 
                DEFAULT_VARIABLE_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.PARENTHESIS_HIGHLIGHT,
            null,
            DEFAULT_PARENTHESIS_HIGHLIGHT_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.KEYWORD_HIGHLIGHT,
            null,
            DEFAULT_KEYWORD_HIGHLIGHT_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.NUMBER,
            Pattern.compile( REGEX_NON_WORD_L + "(?<K>[0-9]+((/|\\.)[0-9]+)?)" + REGEX_NON_WORD_R ),
            DEFAULT_NUMBER_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.SYMBOL,
            Pattern.compile( "(?<K>'[a-zA-Z-_]+)"  ),
            DEFAULT_SYMBOL_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.KEYWORD,
            Pattern.compile( "(?<K>[a-zA-Z-_]+:)" ),
            DEFAULT_KEYWORD_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.HASH,
            Pattern.compile( "(?<K>\\#[a-zA-Z-_]+)" ),
            DEFAULT_HASH_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.SHEBANG,
            Pattern.compile( "^\\#\\!.*\\R" ),
            DEFAULT_SHEBANG_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.STRING,
            Pattern.compile( "\\\"[\\s\\S]*?\\\"", Pattern.MULTILINE ),
            DEFAULT_STRING_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.BLOCK_COMMENT,
            Pattern.compile( "\\#\\|[\\s\\S]*?\\|\\#", Pattern.MULTILINE ),
            DEFAULT_BLOCK_COMMENT_COLOR )); 
        list.add( SyntaxElement.create(
            KawapadSyntaxElementType.LINE_COMMENT,
            Pattern.compile( ";.*$" , Pattern.MULTILINE ),
            DEFAULT_LINE_COMMENT_COLOR ));
        return list;
    }
    
    /**
     * this method may not be necessary anymore.
     * (Fri, 27 Mar 2020 04:36:53 +0900)
     */
    @Override
    public SyntaxElement getDefaultAttributeSet() {
        AttributeSet attr =  SyntaxHighlighter.createAttributeSet( kawapad.getForeground() );
        SyntaxElement element = SyntaxElement.create( KawapadSyntaxElementType.NONE, null, attr );
        SyntaxHighlighter.setSyntaxElement( attr, element ); 
        return element;
    }
}
