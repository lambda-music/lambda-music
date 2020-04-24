package kawapad;

import java.util.ArrayDeque;

public class KawapadParenthesisStack {
    public static class Element {
        public final int mark;
        public final int dot;
        public Element(int mark, int dot) {
            super();
            this.mark = mark;
            this.dot = dot;
        }
    }
    private transient boolean locked = false;
    public boolean isLocked() {
        return this.locked;
    }
    public void setLocked( boolean locked ) {
        this.locked = locked;
    }
    private ArrayDeque<KawapadParenthesisStack.Element> stack = new ArrayDeque<>();
    public void checkSelectionStack() {
        synchronized ( this ) {
            if ( ! locked ) {
                stack.clear();
            }
        }
    }
    public void push( int mark, int dot ) {
        stack.push(new Element(mark, dot));
    }
    public KawapadParenthesisStack.Element pop() {
        return stack.pop();
    }
    public KawapadParenthesisStack.Element peek() {
        return stack.peek();
    }
    public void clear() {
        stack.clear();
    }
    public boolean isEmpty() {
        return stack.isEmpty();
    }
}
