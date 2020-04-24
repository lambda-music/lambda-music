package kawapad;

import javax.swing.text.DefaultStyledDocument;

public final class SyntaxHighlighterStyledDocument extends DefaultStyledDocument {
    public synchronized void callWriteLock() {
        super.writeLock();
    }
    public synchronized void callWriteUnlock() {
        super.writeUnlock();
    }
}