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


    public static Segment getText(Document document) throws InternalError {
        Segment text = new Segment();
        try {
            document.getText( 0, document.getLength(), text );
        } catch ( BadLocationException e ) {
            throw new InternalError(e);
        }
        return text;
    }

    public static final int lookupParenthesis( CharSequence text, int position, int step ) {
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
    
    static void shrinkSelection(KawaPadParenthesisStack stack, CharSequence text, Caret caret) throws InternalError {
        int currDot  = caret.getDot();
        int currMark = caret.getMark();
    
        int leftPos;
        int rightPos;
        int direction;
        if ( currDot < currMark ) {
            leftPos = currDot;
            rightPos = currMark - THE_FINAL_CORRECTION;
            direction = -1;
        } else {
            leftPos = currMark;
            rightPos = currDot - THE_FINAL_CORRECTION;
            direction = +1;
        }
        
        int posL;
        int posR;
        {
            if ( 0 < direction ) {
                posR =     lookupCorrespondingParenthesis2( text, rightPos, -1 , LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP );
                if ( 0<=posR && ( text.charAt( posR ) == ')' ) ) 
                    posL = lookupCorrespondingParenthesis2( text, posR    , -1 , LCP2_STRATEGY_DYNAMIC );
                else
                    posL = -1;
            } else {
                posL =     lookupCorrespondingParenthesis2( text, leftPos , +1 , LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP );
                if ( 0<=posL && ( text.charAt( posL ) == '(' )) 
                    posR = lookupCorrespondingParenthesis2( text, posL    , +1 , LCP2_STRATEGY_DYNAMIC );
                else
                    posR = -1;
            }
            System.err.println( String.format( "leftPos=%d rightPos=%d posL=%d posR=%d", leftPos, rightPos, posL, posR ) );
            if ( (0<=posL) && (0<=posR) && (posL<=posR) ) {
                synchronized ( stack ) {
                    try {
                        stack.setLocked( true );
                        if ( 0 < direction ) {
                            caret.setDot(posL);
                            caret.moveDot(posR + THE_FINAL_CORRECTION);
                        } else {
                            caret.setDot(posR + THE_FINAL_CORRECTION );
                            caret.moveDot(posL );
                        }
                        stack.push(currMark, currDot);
                        return;
                    } finally {
                        stack.setLocked( false );
                    }
                }
            }
        }
    }
}
