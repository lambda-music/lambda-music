package kawapad;

import javax.swing.text.Caret;
import javax.swing.text.Document;

public abstract class CaretTransformer {
    /**
     * The Object that consists a range of a string
     * whereas the both properties "left" "right" are inclusive 
     */
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
        transform( stack, KawapadParenthesisMovement.getText( text ), caret );
    }
    public final void transform( KawaPadParenthesisStack stack, CharSequence text, Caret caret ) {
        int currDot  = caret.getDot();
        int currMark = caret.getMark();

        CaretPos before;
        if ( currDot < currMark ) {
            before = new CaretPos( 
                currDot, 
                currMark - KawapadParenthesisMovement.THE_FINAL_CORRECTION,
                -1 );
        } else if ( currMark < currDot ) {
            before = new CaretPos( 
                currMark, 
                currDot - KawapadParenthesisMovement.THE_FINAL_CORRECTION,
                +1 );
        } else {
            before = new CaretPos( 
                currMark, 
                currDot,
                0 );
        }
        CaretPos after = before.duplicate();
        boolean result = process( text, before, after );
        if (    result && 
                (0<=after.left) && 
                (0<=after.right ) && 
                (after.left  <  text.length() ) && 
                (after.right <= text.length() ) && 
                (after.left <= after.right)) 
        {
            synchronized ( stack ) {
                try {
                    stack.setLocked( true );
                    int correction = after.left == after.right ? 0 : KawapadParenthesisMovement.THE_FINAL_CORRECTION;
                    if ( 0 < after.direction ) {
                        caret.setDot(  after.left );
                        caret.moveDot( after.right + correction );
                    } else if ( after.direction < 0) {
                        caret.setDot(  after.right + correction );
                        caret.moveDot( after.left );
                    } else {
                        caret.setDot(  after.left );
                        caret.moveDot( after.right + correction );
                    }
                    stack.push(currMark, currDot);
                    return;
                } finally {
                    stack.setLocked( false );
                }
            }
        }
    }
    protected abstract boolean process( CharSequence text,  CaretPos before, CaretPos after );
}
