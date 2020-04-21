package kawapad;

import java.awt.Color;

import javax.swing.text.AttributeSet;

import gnu.mapping.Symbol;
import lamu.lib.evaluators.SchemeUtils;

public enum KawapadSyntaxElementType {
    NONE,
    PUNCTUATION,
    VARIABLE,
    KEYWORD_HIGHLIGHT,
    PARENTHESIS_HIGHLIGHT,
    NUMBER,
    SYMBOL,
    KEYWORD,
    HASH,
    SHEBANG,
    STRING,
    LINE_COMMENT,
    BLOCK_COMMENT;
    public static KawapadSyntaxElementType schemeValueOf( Symbol symbol ) {
        String str = SchemeUtils.schemeSymbolToJavaString( symbol ).toUpperCase().replaceAll( "-" , "_" );
        return valueOf( str );
    }
    public static void setDefaultForegroundColor( Symbol symbol, Color color ) {
        KawapadSyntaxElementType type = schemeValueOf( symbol );
        type.setDefaultAttributeSet(
            SyntaxHighlighter.setForeground( 
                color, 
                type.getDefaultAttributeSet()));
    }
    public static void setDefaultBackgroundColor( Symbol symbol, Color color ) {
        KawapadSyntaxElementType type = schemeValueOf( symbol );
        type.setDefaultAttributeSet(
            SyntaxHighlighter.setBackground( 
                color, 
                type.getDefaultAttributeSet()));
    }

    private String getDefaultAttributeSetFieldName() {
        return "DEFAULT_"+ this.name() + "_COLOR";
    }
    public void setDefaultAttributeSet( AttributeSet attr ) {
        try {
            KawapadSyntaxHighlighter.class.getField( getDefaultAttributeSetFieldName() ).set(null, attr );
        } catch (IllegalArgumentException | IllegalAccessException | NoSuchFieldException | SecurityException e) {
            throw new Error( "internal exception",  e );
        }
    }
    public AttributeSet getDefaultAttributeSet() {
        try {
            return (AttributeSet) KawapadSyntaxHighlighter.class.getField( getDefaultAttributeSetFieldName() ).get( null );
        } catch (IllegalArgumentException | IllegalAccessException | NoSuchFieldException | SecurityException e) {
            throw new Error( "internal exception",  e );
        }
    }
    public void setDefaultBackgroundColor( Color color ) {
        this.setDefaultAttributeSet(
            SyntaxHighlighter.setBackground( 
                color, 
                this.getDefaultAttributeSet()));
    }
    public void setDefaultForegroundColor( Color color ) {
        this.setDefaultAttributeSet(
            SyntaxHighlighter.setForeground( 
                color, 
                this.getDefaultAttributeSet()));
    }
}