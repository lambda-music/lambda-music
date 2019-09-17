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
        public CaretPos( Caret cp ) {
            super();
            this.left  = Math.min( cp.getDot(), cp.getMark() );
            this.right = Math.max( cp.getDot(), cp.getMark() )-1;
            this.direction = (int)Math.signum( cp.getDot() - cp.getMark() ); 
        }
        public void setCaret( Caret caret ) {
//          int correction = after.left == after.right ? 0 : KawapadParenthesisMovement.THE_FINAL_CORRECTION;
            int correction = KawapadSelection.THE_FINAL_CORRECTION;
            CaretPos after = this;
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
    public final void transform( KawapadParenthesisStack stack, Document text, Caret caret ) {
        transform( stack, KawapadSelection.getText( text ), caret );
    }
    public final boolean transform( KawapadParenthesisStack stack, CharSequence text, Caret caret ) {
        int currDot  = caret.getDot();
        int currMark = caret.getMark();

        CaretPos before;
        if ( currDot < currMark ) {
            before = new CaretPos( 
                currDot, 
                currMark - KawapadSelection.THE_FINAL_CORRECTION,
                -1 );
        } else if ( currMark < currDot ) {
            before = new CaretPos( 
                currMark, 
                currDot - KawapadSelection.THE_FINAL_CORRECTION,
                +1 );
        } else {
            before = new CaretPos( 
                currMark, 
                currDot,
                0 );
        }
        CaretPos after = before.duplicate();
        boolean result = process( text, before, after );
        if ( result && isValidCaretPos( text, after ) ) {
            synchronized ( stack ) {
                try {
                    stack.setLocked( true );
                    after.setCaret( caret );
                    stack.push(currMark, currDot);
                    return true;
                } finally {
                    stack.setLocked( false );
                }
            }
        } else {
            return false;
        }
    }
    public static boolean isValidCaretPos(CharSequence text, CaretPos caretPos) {
        return 
                (0<=caretPos.left) && 
                (0<=caretPos.right ) && 
                (caretPos.left  <  text.length() ) && 
                (caretPos.right <= text.length() ) && 
                (caretPos.left <= caretPos.right);
    }
    protected abstract boolean process( CharSequence text,  CaretPos before, CaretPos after );
}
