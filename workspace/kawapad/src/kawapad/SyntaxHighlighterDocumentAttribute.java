package kawapad;



import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import javax.swing.text.AbstractDocument.BranchElement;
import javax.swing.text.AbstractDocument.LeafElement;
import javax.swing.text.Element;
import javax.swing.text.StyledDocument;

import kawapad.SyntaxHighlighter.SyntaxElement;

public interface SyntaxHighlighterDocumentAttribute {
    void setCharacterAttributes( int offset, int length, SyntaxElement element, boolean replace );
    SyntaxElement getCharacterAttributes(int offset );
    int getLength();
    void copyTo( SyntaxHighlighterDocumentAttribute da );
    SyntaxHighlighterDocumentAttribute duplicate();
    
    public static class StyleDocumentAttribute implements SyntaxHighlighterDocumentAttribute {
        final StyledDocument document;
        StyleDocumentAttribute(StyledDocument document) {
            this.document = document;
        }
        @Override
        public void setCharacterAttributes(int offset, int length, SyntaxElement element, boolean replace) {
            document.setCharacterAttributes(offset, length, element.getAttributeSet(), replace);
        }
        @Override
        public SyntaxElement getCharacterAttributes(int offset) {
            return SyntaxHighlighter.getSyntaxElement(document.getCharacterElement(offset).getAttributes()); 
        }
        @Override
        public int getLength() {
            return document.getLength() + 1;
        }
        @Override
        public void copyTo( SyntaxHighlighterDocumentAttribute da ) {
            if ( SyntaxHighlighter.DEBUG )
                SyntaxHighlighter.logInfo( "SyntaxHighlighterDocumentAttribute.StyleDocumentAttribute.duplicate()" );
            toDocumentAttribute( document.getRootElements()[0], da );
        }
        static void toDocumentAttribute( Element element, SyntaxHighlighterDocumentAttribute da ) {
            if ( element instanceof BranchElement ) {
                int count = element.getElementCount();
                for ( int i=0; i<count; i++ ) {
                    toDocumentAttribute( element.getElement(i), da );
                }
            } else if ( element instanceof LeafElement ) {
                SyntaxElement syntaxElement = SyntaxHighlighter.getSyntaxElement( element.getAttributes() );
                int startOffset = element.getStartOffset();
                int endOffset = element.getEndOffset();
                da.setCharacterAttributes( startOffset, endOffset-startOffset, syntaxElement, true );
            } else {
                throw new Error( "internal error" );
            }
        }
        @Override
        public SyntaxHighlighterDocumentAttribute duplicate() {
            throw new Error("not implemented");
        }
    }

    public static class ListDocumentAttribute implements SyntaxHighlighterDocumentAttribute {
        final List<SyntaxElement> list;
        private void setSize( int size ) {
            int start = list.size();
            for ( int i=start; i<size; i++ ) {
                list.add(null);
            }
        }
        private void setElement(int offset, int length, SyntaxElement element) {
            for ( int i=0; i<length; i++ ) {
                list.set( offset+i, element );
            }
        }
        public ListDocumentAttribute( List<SyntaxElement> list ) {
            this.list = list;
        }
        @Override
        public void setCharacterAttributes(int offset, int length, SyntaxElement element, boolean replace) {
            setSize( offset + length );
            setElement( offset, length, element );
        }
        @Override
        public SyntaxElement getCharacterAttributes(int offset) {
            return this.list.get(offset);
        }
        @Override
        public int getLength() {
            return list.size();
        }
        @Override
        public void copyTo( SyntaxHighlighterDocumentAttribute da ) {
            throw new Error("not implemented");
        }
        @Override
        public SyntaxHighlighterDocumentAttribute duplicate() {
            throw new Error("not implemented");
        }
    }
    public static class ArrayDocumentAttribute implements SyntaxHighlighterDocumentAttribute {
        final SyntaxElement[] array;
        public ArrayDocumentAttribute( SyntaxElement[] array ) {
            this.array = array;
        }
        @Override
        public void setCharacterAttributes(int offset, int length, SyntaxElement element, boolean replace) {
            Arrays.fill( this.array, offset, offset + length, element );
        }
        @Override
        public SyntaxElement getCharacterAttributes(int offset) {
            return this.array[offset];
        }
        @Override
        public int getLength() {
            return this.array.length;
        }
        @Override
        public void copyTo( SyntaxHighlighterDocumentAttribute da ) {
            if ( SyntaxHighlighter.DEBUG )
                SyntaxHighlighter.logInfo("SyntaxHighlighterDocumentAttribute.ArrayDocumentAttribute.copyTo()");
            int prevPos= -1;
            SyntaxElement prevValue = null;
            for ( int currPos=0; currPos<array.length; currPos++ ) {
                SyntaxElement currElement = array[currPos];
                if ( Objects.equals( currElement, prevValue ) ) {
                } else {
                    da.setCharacterAttributes( prevPos, currPos-prevPos, prevValue, true );
                    prevValue = currElement;
                    prevPos   = currPos; 
                }
            }
        }
        @Override
        public ArrayDocumentAttribute duplicate() {
            SyntaxElement[] copy = new SyntaxElement[ this.array.length ];
            System.arraycopy( this.array, 0, copy, 0 , this.array.length );
            return new ArrayDocumentAttribute( copy );
        }
        public SyntaxElement[] getArray() {
            return array;
        }
        @Override
        public String toString() {
            return Arrays.toString( this.array );
        }
    }
    static void copy( SyntaxHighlighterDocumentAttribute from, ArrayDocumentAttribute to) {
        int length = from.getLength();
        for ( int i=0; i<length; i++) {
             to.setCharacterAttributes( i,1, from.getCharacterAttributes(i), false );
        }
    }
    public static ArrayDocumentAttribute create( SyntaxElement[] array ) {
        return new ArrayDocumentAttribute(array);
    }
    public static SyntaxHighlighterDocumentAttribute duplicate( SyntaxHighlighterDocumentAttribute from ) {
        if ( from instanceof StyleDocumentAttribute ) {
            StyleDocumentAttribute styleFrom = (StyleDocumentAttribute) from;
            SyntaxHighlighterDocumentAttribute ne = SyntaxHighlighterDocumentAttribute.create( styleFrom.getLength() );
            styleFrom.copyTo(ne);
            return ne;            
        } else {
            ArrayDocumentAttribute to = new ArrayDocumentAttribute( new SyntaxElement[ from.getLength() ] );
            copy( from,  to );
            return to;
        }
    }
    public static ArrayDocumentAttribute create( int size ) {
        return new ArrayDocumentAttribute( new SyntaxElement[ size ] );
    }
    public static ArrayDocumentAttribute create( int size, SyntaxElement e ) {
        SyntaxElement[] arr = new SyntaxElement[ size ];
        Arrays.fill(arr, e);
        return new ArrayDocumentAttribute( arr );
    }
    public static StyleDocumentAttribute create( StyledDocument document ) {
//        StyleDocumentStringifier.log( document ); 
        return new StyleDocumentAttribute(document);
    }
}
