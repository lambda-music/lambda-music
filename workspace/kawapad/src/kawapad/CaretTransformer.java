package kawapad;

import javax.swing.text.Caret;

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
    }
    public final void execute( KawaPadParenthesisStack stack, CharSequence text, Caret caret ) {
        int currDot  = caret.getDot();
        int currMark = caret.getMark();

        CaretPos current;
        if ( currDot < currMark ) {
            current = new CaretPos( 
                currDot, 
                currMark - SchemeParentheses.THE_FINAL_CORRECTION,
                -1 );
        } else if ( currMark < currDot ) {
            current = new CaretPos( 
                currMark, 
                currDot - SchemeParentheses.THE_FINAL_CORRECTION,
                +1 );
        } else {
            current = new CaretPos( 
                currMark, 
                currDot,
                0 );
        }
        CaretPos after = process( text, current );
        
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
    public abstract CaretPos process( CharSequence text,  CaretPos current );
}
