package kawapad;

import java.awt.Color;
import java.util.regex.Pattern;

import javax.swing.text.AttributeSet;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter.HighlightPainter;

public interface SyntaxElement {
    public static SyntaxElement create( Object name, Pattern pattern, AttributeSet attributeSet ) {
        return new SyntaxElement.Default( name, pattern, attributeSet );
    }

    /*
     * It appeared that setting AttributeSet destroy the consistency. The set
     * AttributeSet object does not exist in Editor's AttributeSet list; it is
     * necessary to replace all AttributeSet objects in the list. If the replacemet
     * is not done, the color modification done to this object will not reflect to
     * the editor.
     */
    Object getName();
    void setPattern(Pattern pattern);
    Pattern getPattern();
    AttributeSet getAttributeSet();
    void setAttributeSet(AttributeSet attributeSet);
    HighlightPainter getHighlightPainter();
    
    default public Color getForegroundColor() {
        return SyntaxHighlighter.getForegroundColor( getAttributeSet() );
    }
    default public Color getBackgroundColor() {
        return SyntaxHighlighter.getBackgroundColor( getAttributeSet() );
    }
    default public void setBackgroundColor(Color color) {
        this.setAttributeSet( SyntaxHighlighter.setBackground( color, getAttributeSet() ));
    }
    default public void setForegroundColor(Color color) {
        this.setAttributeSet( SyntaxHighlighter.setForeground( color, getAttributeSet() ));
    }
    

    static final class Default implements SyntaxElement {
        private Object name;
        private Pattern pattern;
        private AttributeSet attributeSet;
        private HighlightPainter highlightPainter;
        public Default( Object name, Pattern pattern, AttributeSet attributeSet ) {
            super();
            this.name = name;
            this.pattern = pattern;
            this.attributeSet = SyntaxHighlighter.setSyntaxElement( attributeSet , this );
            this.updateHighlightPainter();
        }
        public void updateHighlightPainter() {
            Color color = SyntaxHighlighter.getBackgroundColor( this.getAttributeSet() );
            if ( color != null )
                this.highlightPainter =  
                    new DefaultHighlighter.DefaultHighlightPainter( color );
            else 
                this.highlightPainter =  
                    new DefaultHighlighter.DefaultHighlightPainter( Color.CYAN );
        }
        @Override
        public Object getName() {
            return name;
        }
        @Override
        public void setPattern(Pattern pattern) {
            this.pattern = pattern;
        }
        @Override
        public Pattern getPattern() {
            return pattern;
        }
        @Override
        public void setAttributeSet(AttributeSet attributeSet) {
            this.attributeSet = attributeSet;
        }
        @Override
        public AttributeSet getAttributeSet() {
            return attributeSet;
        }
        @Override
        public HighlightPainter getHighlightPainter() {
            return this.highlightPainter;
        }
        @Override
        public void setBackgroundColor(Color color) {
            SyntaxElement.super.setBackgroundColor(color);
            this.updateHighlightPainter();
        }
        @Override
        public void setForegroundColor(Color color) {
            SyntaxElement.super.setForegroundColor(color);
            this.updateHighlightPainter();
        }
        @Override
        public String toString() {
            return String.format("%s",name);
        }
    }
}