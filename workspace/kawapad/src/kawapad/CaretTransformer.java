package kawapad;

import javax.swing.text.Caret;
import javax.swing.text.Document;

public abstract class CaretTransformer {
    public static class CaretPos {
        public int left;
        public int right;
        public int direction;
        public int getLeft() {
            return left;
        }
        public void setLeft(int left) {
            this.left = left;
        }
        public int getRight() {
            return right;
        }
        public void setRight(int right) {
            this.right = right;
        }
        public int getDirection() {
            return direction;
        }
        public void setDirection(int direction) {
            this.direction = direction;
        }
        public CaretPos( int left, int right, int direction ) {
            super();
            this.left  = left;
            this.right = right;
            this.direction = direction;
        }
        public CaretPos( CaretPos cp ) {
            super();
            this.left  = cp.left;
            this.right = cp.right;
            this.direction = cp.direction;
        }
        @Override
        public String toString() {
            if ( 0<direction ) {
                return String.format( "CaretPos %d=>%d", left,right );
            } else if ( direction < 0 ) {
                return String.format( "CaretPos %d<=%d", left,right );
            } else {
                return String.format( "CaretPos %d==%d", left,right );
            }
        }
        public CaretPos duplicate() {
            return new CaretPos( this );
        }
    }
    public final void transform( KawaPadParenthesisStack stack, Document text, Caret caret ) {
        transform( stack, SchemeParentheses.getText( text ), caret );
    }
    public final void transform( KawaPadParenthesisStack stack, CharSequence text, Caret caret ) {
        int currDot  = caret.getDot();
        int currMark = caret.getMark();

        CaretPos before;
        if ( currDot < currMark ) {
            before = new CaretPos( 
                currDot, 
                currMark - SchemeParentheses.THE_FINAL_CORRECTION,
                -1 );
        } else if ( currMark < currDot ) {
            before = new CaretPos( 
                currMark, 
                currDot - SchemeParentheses.THE_FINAL_CORRECTION,
                +1 );
        } else {
            before = new CaretPos( 
                currMark, 
                currDot,
                0 );
        }
        CaretPos after = before.duplicate();
        process( text, before, after );
        
        if ( (0<=after.left) && (0<=after.right ) && (after.getLeft() <= after.getRight() ) ) {
            synchronized ( stack ) {
                try {
                    stack.setLocked( true );
                    if ( 0 < after.direction ) {
                        caret.setDot(  after.getLeft() );
                        caret.moveDot( after.getRight() + SchemeParentheses.THE_FINAL_CORRECTION);
                    } else if ( after.direction < 0) {
                        caret.setDot(  after.getRight() + SchemeParentheses.THE_FINAL_CORRECTION );
                        caret.moveDot( after.getLeft() );
                    } else {
                        caret.setDot(  after.getLeft() );
                        caret.moveDot( after.getRight() + SchemeParentheses.THE_FINAL_CORRECTION );
                    }
                    stack.push(currMark, currDot);
                    return;
                } finally {
                    stack.setLocked( false );
                }
            }
        }
    }
    protected abstract void process( CharSequence text,  CaretPos before, CaretPos after );
}
