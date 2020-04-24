package kawapad;

import javax.swing.text.Document;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.ViewFactory;

class KawapadStyledEditorKit extends StyledEditorKit {
    @Override
    public ViewFactory getViewFactory() {
        return new KawapadStyledEditorkitViewFactory(); 
    }

    @Override
    public Document createDefaultDocument() {
        return new SyntaxHighlighterStyledDocument();
    }
}