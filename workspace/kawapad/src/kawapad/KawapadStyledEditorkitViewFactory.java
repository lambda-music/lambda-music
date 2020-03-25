package kawapad;

import java.awt.Graphics;
import java.awt.Shape;

import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.LabelView;
import javax.swing.text.ParagraphView;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

class KawapadStyledEditorkitViewFactory implements ViewFactory {
    public View create(Element elem) {
        String kind = elem.getName();
        if (kind != null) {
            if (kind.equals(AbstractDocument.ContentElementName)) {
                return new LabelView(elem) {
                    @Override
                    public void paint(Graphics g, Shape a) {
                        if ( this.getElement().getElementCount() == 1 ) {
                            Document d = this.getElement().getDocument();
                            int startPos = d.getStartPosition().getOffset();
                            int endPos = d.getEndPosition().getOffset();
                            
                            try {
                                Kawapad.logInfo( d.getText( startPos,  endPos - startPos ) );
                            } catch (BadLocationException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            
                        }
                        super.paint(g, a);
                        
                    }
                };
            } else if (kind.equals(AbstractDocument.ParagraphElementName)) {
                return new ParagraphView(elem);
            } else if (kind.equals(AbstractDocument.SectionElementName)) {
                return new BoxView(elem, View.Y_AXIS);
            } else if (kind.equals(StyleConstants.ComponentElementName)) {
                return new ComponentView(elem);
            } else if (kind.equals(StyleConstants.IconElementName)) {
                return new IconView(elem);
            }
        }

        // default to text display
        return new LabelView(elem);
    }
}