/*
 * Kawapad written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Metro Musical Sequencing Framework. 
 * 
 * Kawapad is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Kawapad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Kawapad.  If not, see <https://www.gnu.org/licenses/>.
 */

package kawapad;

import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.Segment;

/**
 *  October 3, 2018 at 9:52:22 PM
 */
public class SchemeParentheses {
    public static Segment getText(Document document) throws InternalError {
        Segment text = new Segment();
        try {
            document.getText( 0, document.getLength(), text );
        } catch ( BadLocationException e ) {
            throw new InternalError(e);
        }
        return text;
    }

    static final int THE_FINAL_CORRECTION = 1;
    static boolean expandSelectedParentheses(Kawapad textPane) {
        return expandSelectedParentheses( textPane.getParenthesisStack(), 
            getText( textPane.getDocument() ), 
            textPane.getCaret() );
    }
    
    static boolean expandSelectedParentheses( KawaPadParenthesisStack stack, CharSequence text, Caret caret ) {
        int currDot  = caret.getDot();
        int currMark = caret.getMark();
        int leftPos;
        int rightPos;
        if ( currDot < currMark ) {
            leftPos = currDot;
            rightPos = currMark - THE_FINAL_CORRECTION;
        } else {
            leftPos = currMark;
            rightPos = currDot - THE_FINAL_CORRECTION;
        }
        
        // if there is a selection area now, it is to expand one on the left side.
        if ( leftPos != rightPos )
            rightPos ++;
        else if ( text.charAt(leftPos) == '(') {
            leftPos ++;
            rightPos++;
        }
        
        if ( leftPos < 0 )
            leftPos = 0;
        else if ( text.length() < leftPos )
            leftPos = text.length();
        if ( rightPos < 0 )
            rightPos = 0;
        else if ( text.length() < rightPos )
            rightPos = text.length();
        
        CharSequence left_leftString   = text.subSequence(0, leftPos);
        CharSequence left_rightString  = text.subSequence(leftPos,text.length());
        CharSequence right_leftString  = text.subSequence(0, rightPos);
        CharSequence right_rightString = text.subSequence(rightPos,text.length());
        int posL;
        int posR;
        int diff; // the length in char of the inserted text in the middle of the argument string.
        
        /*
         * We will search twice :
         *  - once for simply looking for the corresponding parenthesis.
         *   - once we presume that we are in a block of quotations. We well try to close it before searching
         *     the corresponding parenthesis; otherwise, we will search for the next half quotation which
         *     is not what we want.
         */
        {
            // the first search
            diff = 1;
            posL = SchemeParenthesisParser.lookupCorrespondingParenthesis( left_leftString + ")" + left_rightString, leftPos );
            posR = SchemeParenthesisParser.lookupCorrespondingParenthesis( right_leftString + "(" + right_rightString, rightPos );
            
            if ( 0<=posL && 0<=posR ) {
                synchronized ( stack ) {
                    try {
                        stack.setLocked( true );
                        caret.setDot(posL);
                        caret.moveDot(posR-diff + THE_FINAL_CORRECTION);
                        stack.push( currMark, currDot );
                        return true;
                    } finally {
                        stack.setLocked( false );
                    }
                }
            }
        }
        {
            // the second search
            diff = 2;
            posL = SchemeParenthesisParser.lookupCorrespondingParenthesis( left_leftString + "(\"" + left_rightString, leftPos   );
            posR = SchemeParenthesisParser.lookupCorrespondingParenthesis( right_leftString + "\")" + right_rightString, rightPos +1 );
            if ( 0<=posL && 0<=posR ) {
                synchronized ( stack ) {
                    try {
                        stack.setLocked( true );
                        caret.setDot(posL-diff);
                        caret.moveDot(posR + THE_FINAL_CORRECTION);
                        stack.push( currMark, currDot );
                        return true;
                    } finally {
                        stack.setLocked( false );
                    }
                }
            }
            return false;
        }
    }   

    
    public interface CharSelector {
        boolean select( char ch );
    }
    
    public static final int lookup( CharSequence text, CharSelector selector, int position, int step ) {
        if ( text == null )
            throw new NullPointerException();
        
        if ( step == 0 )
            throw new IllegalArgumentException();
        
        while ( 0<= position && position < text.length() ) {
            char c = text.charAt( position );
            if ( selector.select( c ) ) {
                return position;
            } else {
                position += step;
            }
        }
        return -1;
    }


    static final CharSelector parenthesesSelector = new CharSelector() {
        @Override
        public boolean select(char ch) {
            return ch == '(' || ch == ')';
        }
    };
    public static final int lookupParenthesis( CharSequence text, int position, int step ) {
        return lookup( text, parenthesesSelector, position, step );
    }
    public static final int lookupParenthesisOld( CharSequence text, int position, int step ) {
        if ( text == null )
            throw new NullPointerException();
        
        if ( step == 0 )
            throw new IllegalArgumentException();
        
        while ( 0<= position && position < text.length() ) {
            char c = text.charAt( position );
            if ( c == '(' || c == ')' ) {
                return position;
            } else {
                position += step;
            }
        }
        return -1;
    }
    
    
    public static final int LCP2_STRATEGY_DYNAMIC = -1024;
    public static final int LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP = 1;
    public static final int LCP2_STRATEGY_CORRESPONDING_PARENTHESIS_JUMP = 2;

    public static int lookupCorrespondingParenthesis2(CharSequence text, int currDot, int direction, int constantStrategy ) throws InternalError {
        if ( currDot < 0 ) 
            currDot = 0;
        if ( text.length() <= currDot )
            currDot = text.length() -1;
            
        char currentChar = text.charAt( currDot );
        int totalOffset = 0;
        
        // 0 : do nothing
        // 1 : look for "(" or ")"
        // 2 : look for the corresponding parenthesis.
        int strategy;
        
        // constantStrategy < 0 means dynamic strategy 
        // (Tue, 13 Aug 2019 21:59:23 +0900)
        if ( constantStrategy == LCP2_STRATEGY_DYNAMIC ) {
            switch ( currentChar ) {
                case '(' :
                    if ( direction < 0 ) {
                        totalOffset += -1;
                        strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP ;
                    } else {
                        totalOffset +=  0;
                        strategy = LCP2_STRATEGY_CORRESPONDING_PARENTHESIS_JUMP;
                    }
                    break;
                case ')' : 
                    if ( direction < 0 ) {
                        totalOffset +=  0;
                        strategy = LCP2_STRATEGY_CORRESPONDING_PARENTHESIS_JUMP;
                    } else {
                        totalOffset +=  1;
                        strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                    }
                    break;
                default :
                    strategy = 1;
            }
        } else {
            switch ( currentChar ) {
                case '(' :
                    if ( direction < 0 ) {
                        totalOffset += -1;
                        strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP ;
                    } else {
                        totalOffset +=  1;
                        strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                    }
                    break;
                case ')' : 
                    if ( direction < 0 ) {
                        totalOffset +=  -1;
                        strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                    } else {
                        totalOffset +=  1;
                        strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                    }
                    break;
                default :
                    strategy = 1;
            }
        }
        
        int newDot=-1;
        
        switch ( strategy ) {
            case 0 : 
                // do nothing
                newDot = currDot + totalOffset;
                break;
            case LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP : { 
                // strategy 1 : no parenthesis is found under the cursor.
                int pos = lookupParenthesis(text, currDot + totalOffset, direction );
                if ( 0<=pos ) {
                    newDot = pos;
                } else {
                    if ( direction < 0 )
                        newDot = 0;
                    else
                        newDot = text.length();
                }
                break;
            }
            case LCP2_STRATEGY_CORRESPONDING_PARENTHESIS_JUMP : { 
                // strategy 0: a parenthesis is found under the cursor.
                int pos = SchemeParenthesisParser.lookupCorrespondingParenthesis( text, currDot + totalOffset );
                if ( 0<=pos ) {
                    newDot = pos;
                } else {
                    newDot = currDot + totalOffset;
                }
                break;
            }
            default :
                throw new InternalError();
        }
        return newDot;
    }
    
    public static int recursiveIndexOf( CharSequence text, int fromIndex, int direction, char descendChar, char ascendChar ) {
        if ( direction == 0 )
            throw new IllegalArgumentException();
        int depth =1;
        for ( int i=fromIndex; 0<=i && i<text.length(); i+=direction ) {
            char ch = text.charAt( i );
            if ( ch == descendChar )
                depth ++;
            else if ( ch == ascendChar )
                depth --;
            if ( depth <= 0 ) {
                return i;
            }
        }
        return -1;
    }
    
    static class ExpandParenthesisSelector extends CaretTransformer {
        @Override
        protected boolean process(CharSequence text, CaretPos before, CaretPos after ) {
            // if there is a selection area now, it is to expand one on the left side.
            if ( after.left != after.right )
                after.right ++;
            else if ( text.charAt(after.left) == '(') {
                after.left ++;
                after.right++;
            }
            
            if ( after.left < 0 )
                after.left = 0;
            else if ( text.length() < after.left )
                after.left = text.length();
            if ( after.right < 0 )
                after.right = 0;
            else if ( text.length() < after.right )
                after.right = text.length();
            
            CharSequence left_leftString   = text.subSequence(0, after.left);
            CharSequence left_rightString  = text.subSequence(after.left,text.length());
            CharSequence right_leftString  = text.subSequence(0, after.right);
            CharSequence right_rightString = text.subSequence(after.right,text.length());
            int diff; // the length in char of the inserted text in the middle of the argument string.
            
            /*
             * We will search twice :
             *  - once for simply looking for the corresponding parenthesis.
             *   - once we presume that we are in a block of quotations. We well try to close it before searching
             *     the corresponding parenthesis; otherwise, we will search for the next half quotation which
             *     is not what we want.
             */
            {
                // the first search
                diff = 1;
                after.left   = SchemeParenthesisParser.lookupCorrespondingParenthesis( left_leftString  + ")" + left_rightString,  after.left  );
                after.right  = SchemeParenthesisParser.lookupCorrespondingParenthesis( right_leftString + "(" + right_rightString, after.right );
                after.right -=diff;
                if ( 0<=after.left && 0<=after.right ) {
                    if ( Math.abs( after.left - before.left ) < Math.abs( after.right - before.right ) ) {
                        after.direction = -1;
                    } else {
                        after.direction =  1;
                    }
                    return true;
                }
            }
            {
                // the second search
                diff = 2;
                after.left  = SchemeParenthesisParser.lookupCorrespondingParenthesis( left_leftString + "(\"" + left_rightString, after.left   );
                after.right = SchemeParenthesisParser.lookupCorrespondingParenthesis( right_leftString + "\")" + right_rightString, after.right +1 );
                after.right -=diff;
                if ( 0<=after.left && 0<=after.right ) {
                    if ( Math.abs( after.left - before.left ) < Math.abs( after.right - before.right ) ) {
                        after.direction = -1;
                    } else {
                        after.direction =  1;
                    }
                    return true;
                }
            }
            return false;
        }            
    }
    
    
    static class ShrinkParenthesisSelector extends CaretTransformer {
        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after ) {
            if ( 0 < before.direction ) {
                after.right     = lookupCorrespondingParenthesis2( text, before.right, -1 , LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP );
                if ( 0<=after.right && ( text.charAt( after.right ) == ')' ) ) 
                    after.left  = lookupCorrespondingParenthesis2( text, after.right  , -1 , LCP2_STRATEGY_DYNAMIC );
                else
                    after.left  = -1;
            } else {
                after.left      = lookupCorrespondingParenthesis2( text, before.left , +1 , LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP );
                if ( 0<=after.left && ( text.charAt( after.left ) == '(' )) 
                    after.right = lookupCorrespondingParenthesis2( text, after.left   , +1 , LCP2_STRATEGY_DYNAMIC );
                else
                    after.right = -1;
            }
            System.err.println(
                String.format( "leftPos=%d rightPos=%d posL=%d posR=%d", 
                    before.left, before.right, 
                    before.left, before.right ));
            
            if ( after.right < 0 || after.left < 0 ) {
                after.left = before.left + (( before.right - before.left ) / 2);
                after.right = after.left;
            }
            
            return true;
        }
    }
    static int leftWordEdgePos( CharSequence text, int pos ) {
        int p = lookup( text, LispWordSelectionTransformer.parenthesesSelector, pos , -1 );
        if ( 0<=p )
            return p+1;
        else
            return -1;
    }
    static int rightWordEdgePos( CharSequence text, int pos ) {
        int p = lookup( text, LispWordSelectionTransformer.parenthesesSelector, pos , +1 );
        if ( 0<=p )
            return p-1;
        else
            return -1;
    }
    
    static class SideParenthesisSelector extends CaretTransformer {
        int direction;
        public SideParenthesisSelector(int direction) {
            super();
            this.direction = direction;
        }
        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after) {
            Kawapad.logInfo( "SideParenthesisSelector:" + before );
            if ( 0< direction ) {
                after.left      = recursiveIndexOf( text, before.right+1, +1 , ')', '('  );
                if ( 0<=after.left ) 
                    after.right = lookupCorrespondingParenthesis2( text, after.left     , +1, LCP2_STRATEGY_DYNAMIC );
                else
                    after.right = -1;
            } else {
                after.right     = recursiveIndexOf( text, before.left-1, -1, '(', ')' );
                if ( 0<=after.right ) 
                    after.left  = lookupCorrespondingParenthesis2( text, after.right    , -1, LCP2_STRATEGY_DYNAMIC );
                else
                    after.left  = -1;
            }
            return true;
        }
    }
    
    static abstract class LispWordSelectionTransformer extends CaretTransformer {
        static final CharSelector parenthesesSelector = new CharSelector() {
            @Override
            public boolean select(char ch) {
                return Character.isWhitespace( ch ) ||
                        ch == '(' ||
                        ch == ')' ||
                        false;                        
            }
        };
        static final CharSelector nagatedParenthesesSelector = new CharSelector() {
            @Override
            public boolean select(char ch) {
                return ! (
                        Character.isWhitespace( ch ) ||
                        ch == '(' ||
                        ch == ')' ||
                        false
                        );                        
            }
        };
    }
    static class SelectCurrentWordTransformer extends LispWordSelectionTransformer {
        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after) {
            after.right = lookup( text, parenthesesSelector, before.right, +1 );
            after.left  = lookup( text, parenthesesSelector, before.left , -1 );
            
            if ( 0<=after.right && 0<=after.left ) {
                after.right --;
                after.left ++;
            }
                
            return true;
        }
    }
    static class SelectRightLispWordTransformer extends LispWordSelectionTransformer {
        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after) {
            after.left  = lookup( text, nagatedParenthesesSelector, before.right+1, +1 );
            if ( 0<= after.left ) 
                after.right = lookup( text, parenthesesSelector, after.left , +1 );
            else
                after.right = -1;
            
            if ( 0<=after.right && 0<=after.left ) {
                after.right --;
                return true;
            } else {
                return false;
            }
        }
    }
    static class SelectLeftLispWordTransformer extends LispWordSelectionTransformer {
        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after) {
            after.right = lookup( text, nagatedParenthesesSelector, before.left-1, -1 );
            if ( 0<= after.right ) 
                after.left = lookup( text, parenthesesSelector, after.right, -1 );
            else
                after.left = -1;
            
            if ( 0<=after.right && 0<=after.left ) {
                after.left ++;
                return true;
            } else {
                return false;
            }
        }
    }
}
