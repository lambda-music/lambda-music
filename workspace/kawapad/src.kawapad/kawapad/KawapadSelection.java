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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.text.BadLocationException;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.Segment;

import kawapad.CaretTransformer.CaretPos;

/**
 * October 3, 2018 at 9:52:22 PM
 */
public class KawapadSelection {
    public static Segment getText(Document document) throws InternalError {
        Segment text = new Segment();
        try {
            document.getText(0, document.getLength(), text);
        } catch (BadLocationException e) {
            throw new InternalError(e);
        }
        return text;
    }

    static final int THE_FINAL_CORRECTION = 1;

    public static boolean expandSelectedParentheses(Kawapad textPane) {
        return expandSelectedParentheses(
            textPane.getParenthesisStack(),
            getText(textPane.getDocument()),
            textPane.getCaret());
    }
    public static void expandSelectedParenthesesToTheOuterMost( Kawapad kawapad ) {
        // In order to avoid entering an infinite loop,
        // we use /for/ loop instead of /while/ loop;
        for ( int i=0; i<100; i++ ) {
            if ( expandSelectedParentheses( kawapad ) ) {
                break;
            }
        }
    }

    static boolean expandSelectedParentheses(KawapadParenthesisStack stack, CharSequence text, Caret caret) {
        int currDot = caret.getDot();
        int currMark = caret.getMark();
        int leftPos;
        int rightPos;
        if (currDot < currMark) {
            leftPos = currDot;
            rightPos = currMark - THE_FINAL_CORRECTION;
        } else {
            leftPos = currMark;
            rightPos = currDot - THE_FINAL_CORRECTION;
        }

        // if there is a selection area now, it is to expand one on the left side.
        if (leftPos != rightPos)
            rightPos++;
        else if (text.charAt(leftPos) == '(') {
            leftPos++;
            rightPos++;
        }

        if (leftPos < 0)
            leftPos = 0;
        else if (text.length() < leftPos)
            leftPos = text.length();
        if (rightPos < 0)
            rightPos = 0;
        else if (text.length() < rightPos)
            rightPos = text.length();

        CharSequence left_leftString = text.subSequence(0, leftPos);
        CharSequence left_rightString = text.subSequence(leftPos, text.length());
        CharSequence right_leftString = text.subSequence(0, rightPos);
        CharSequence right_rightString = text.subSequence(rightPos, text.length());
        int posL;
        int posR;
        int diff; // the length in char of the inserted text in the middle of the argument string.

        /*
         * We will search twice : - once for simply looking for the corresponding
         * parenthesis. - once we presume that we are in a block of quotations. We well
         * try to close it before searching the corresponding parenthesis; otherwise, we
         * will search for the next half quotation which is not what we want.
         */
        {
            // the first search
            diff = 1;
            posL = SchemeParenthesisParser.lookupCorrespondingParenthesis(left_leftString + ")" + left_rightString,
                leftPos);
            posR = SchemeParenthesisParser.lookupCorrespondingParenthesis(right_leftString + "(" + right_rightString,
                rightPos);

            if (0 <= posL && 0 <= posR) {
                synchronized (stack) {
                    try {
                        stack.setLocked(true);
                        caret.setDot(posL);
                        caret.moveDot(posR - diff + THE_FINAL_CORRECTION);
                        stack.push(currMark, currDot);
                        return true;
                    } finally {
                        stack.setLocked(false);
                    }
                }
            }
        }
        {
            // the second search
            diff = 2;
            posL = SchemeParenthesisParser.lookupCorrespondingParenthesis(left_leftString + "(\"" + left_rightString,
                leftPos);
            posR = SchemeParenthesisParser.lookupCorrespondingParenthesis(right_leftString + "\")" + right_rightString,
                rightPos + 1);
            if (0 <= posL && 0 <= posR) {
                synchronized (stack) {
                    try {
                        stack.setLocked(true);
                        caret.setDot(posL - diff);
                        caret.moveDot(posR + THE_FINAL_CORRECTION);
                        stack.push(currMark, currDot);
                        return true;
                    } finally {
                        stack.setLocked(false);
                    }
                }
            }
            return false;
        }
    }

    public interface CharSelector {
        boolean select(char ch);
    }

    /**
     * This method looks up the specified character from the passed CharSequence
     * object. When the specified character was not found, returns -1 when the
     * "step" is lower than zero and returns text.length() when the "step" is higher
     * than zero. This throws {@link IllegalArgumentException} when step is zero.
     */
    public static final int lookup(CharSequence text, CharSelector selector, int position, int step) {
        if (text == null)
            throw new NullPointerException();

        if (step == 0)
            throw new IllegalArgumentException();

        int length = text.length();
        while (0 <= position && position < length) {
            char c = text.charAt(position);
            if (selector.select(c)) {
                return position;
            } else {
                position += step;
            }
        }
        return position;
    }

    static final CharSelector parenthesesSelector = new CharSelector() {
        @Override
        public boolean select(char ch) {
            return ch == '(' || ch == ')';
        }
    };

    public static final int lookupParenthesis(CharSequence text, int position, int step) {
        return lookup(text, parenthesesSelector, position, step);
    }

    public static final int lookupParenthesisOld(CharSequence text, int position, int step) {
        if (text == null)
            throw new NullPointerException();

        if (step == 0)
            throw new IllegalArgumentException();

        while (0 <= position && position < text.length()) {
            char c = text.charAt(position);
            if (c == '(' || c == ')') {
                return position;
            } else {
                position += step;
            }
        }
        return -1;
    }

    static int lookupCorrespondingParenthesis1(CharSequence text, int currDot) {
        return SchemeParenthesisParser.lookupCorrespondingParenthesis(text, currDot);
    }

    public static final int LCP2_STRATEGY_DYNAMIC = -1024;
    public static final int LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP = 1;
    public static final int LCP2_STRATEGY_CORRESPONDING_PARENTHESIS_JUMP = 2;

    public static int lookupCorrespondingParenthesis(CharSequence text, int currDot, int direction) {
        return lookupCorrespondingParenthesis2(text, currDot, direction, LCP2_STRATEGY_DYNAMIC);
    }

    public static int lookupCorrespondingParenthesis2(CharSequence text, int currDot, int direction,
        int constantStrategy) {
        if (currDot < 0)
            currDot = 0;
        if (text.length() <= currDot)
            currDot = text.length() - 1;

        char currentChar = text.charAt(currDot);
        int totalOffset = 0;

        // 0 : do nothing
        // 1 : look for "(" or ")"
        // 2 : look for the corresponding parenthesis.
        int strategy;

        // constantStrategy < 0 means dynamic strategy
        // (Tue, 13 Aug 2019 21:59:23 +0900)
        if (constantStrategy == LCP2_STRATEGY_DYNAMIC) {
            switch (currentChar) {
            case '(':
                if (direction < 0) {
                    totalOffset += -1;
                    strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                } else {
                    totalOffset += 0;
                    strategy = LCP2_STRATEGY_CORRESPONDING_PARENTHESIS_JUMP;
                }
                break;
            case ')':
                if (direction < 0) {
                    totalOffset += 0;
                    strategy = LCP2_STRATEGY_CORRESPONDING_PARENTHESIS_JUMP;
                } else {
                    totalOffset += 1;
                    strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                }
                break;
            default:
                strategy = 1;
            }
        } else {
            switch (currentChar) {
            case '(':
                if (direction < 0) {
                    totalOffset += -1;
                    strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                } else {
                    totalOffset += 1;
                    strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                }
                break;
            case ')':
                if (direction < 0) {
                    totalOffset += -1;
                    strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                } else {
                    totalOffset += 1;
                    strategy = LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP;
                }
                break;
            default:
                strategy = 1;
            }
        }

        int newDot = -1;

        switch (strategy) {
        case 0:
            // do nothing
            newDot = currDot + totalOffset;
            break;
        case LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP: {
            // strategy 1 : no parenthesis is found under the cursor.
            int pos = lookupParenthesis(text, currDot + totalOffset, direction);
            if (0 <= pos) {
                newDot = pos;
            } else {
                if (direction < 0)
                    newDot = 0;
                else
                    newDot = text.length();
            }
            break;
        }
        case LCP2_STRATEGY_CORRESPONDING_PARENTHESIS_JUMP: {
            // strategy 0: a parenthesis is found under the cursor.
            int pos = lookupCorrespondingParenthesis1(text, currDot);
            if (0 <= pos) {
                newDot = pos;
            } else {
                newDot = currDot + totalOffset;
            }
            break;
        }
        default:
            throw new InternalError();
        }
        return newDot;
    }

    public static int recursiveIndexOf(CharSequence text, int fromIndex, int direction, char descendChar,
        char ascendChar) {
        if (direction == 0)
            throw new IllegalArgumentException();
        boolean ascending = false;
        int depth = 1;
        for (int i = fromIndex; 0 <= i && i < text.length(); i += direction) {
            char ch = text.charAt(i);
            if (ch == descendChar) {
                // we only look for the trench which is next to the current trench.
                // (Mon, 09 Sep 2019 06:47:37 +0900)
                if (ascending) {
                    return -1;
                }
                depth++;

            } else if (ch == ascendChar) {
                depth--;
                ascending = true;
            }

            if (depth <= 0) {
                return i;
            }
        }
        return -1;
    }

    static final ExpandParenthesisSelector PARENTHESES_EXPAND_TRANSFORMER = new ExpandParenthesisSelector(); 
    static class ExpandParenthesisSelector extends CaretTransformer {
        @Override
        protected boolean process(CharSequence text, CaretPos before, CaretPos after) {
            CaretPos start = before.duplicate();
            boolean selection = false;
            // if there is a selection area now, it is to expand one on the left side.
            if (start.left != start.right) {
                start.right++;
                selection = true;
            } else if (text.charAt(start.left) == '(') {
                start.left++;
                start.right++;
            }

            if (start.left < 0)
                start.left = 0;
            else if (text.length() < start.left)
                start.left = text.length();
            if (start.right < 0)
                start.right = 0;
            else if (text.length() < start.right)
                start.right = text.length();

            CharSequence left_leftString = text.subSequence(0, start.left);
            CharSequence left_rightString = text.subSequence(start.left, text.length());
            CharSequence right_leftString = text.subSequence(0, start.right);
            CharSequence right_rightString = text.subSequence(start.right, text.length());
            int diff; // the length in char of the inserted text in the middle of the argument string.

            /*
             * We will search twice : - once for simply looking for the corresponding
             * parenthesis. - once we presume that we are in a block of quotations. We well
             * try to close it before searching the corresponding parenthesis; otherwise, we
             * will search for the next half quotation which is not what we want.
             */
            {
                // the first search
                diff = 1;
                after.left = SchemeParenthesisParser
                    .lookupCorrespondingParenthesis(left_leftString + ")" + left_rightString, start.left);
                after.right = SchemeParenthesisParser
                    .lookupCorrespondingParenthesis(right_leftString + "(" + right_rightString, start.right);
                after.right -= diff;
                if (0 <= after.left && 0 <= after.right) {
                    // Change the direction of the selection, if it is not selected; otherwise
                    // keep the current direction of the selection.
                    if (!selection) {
                        if (Math.abs(after.left - before.left) < Math.abs(after.right - before.right)) {
                            after.direction = -1;
                        } else {
                            after.direction = 1;
                        }
                    }
                    return true;
                }
            }
            {
                // the second search
                diff = 2;
                after.left = SchemeParenthesisParser
                    .lookupCorrespondingParenthesis(right_leftString + "\")" + right_rightString, start.right + 1);
                after.right = SchemeParenthesisParser
                    .lookupCorrespondingParenthesis(left_leftString + "(\"" + left_rightString, start.left);
                after.right -= diff;
                if (0 <= after.left && 0 <= after.right) {
                    // Change the direction of the selection, if it is not selected; otherwise
                    // keep the current direction of the selection.
                    if (!selection) {
                        if (Math.abs(after.left - before.left) < Math.abs(after.right - before.right)) {
                            after.direction = -1;
                        } else {
                            after.direction = 1;
                        }
                    }
                    return true;
                }
            }
            return false;
        }
    }

    static class ShrinkParenthesisSelector extends CaretTransformer {
        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after) {
            if (0 < before.direction) {
                after.right = lookupCorrespondingParenthesis2(text, before.right, -1,
                    LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP);
                if (0 <= after.right && (text.charAt(after.right) == ')'))
                    after.left = lookupCorrespondingParenthesis2(text, after.right, -1, LCP2_STRATEGY_DYNAMIC);
                else
                    after.left = -1;
            } else {
                after.left = lookupCorrespondingParenthesis2(text, before.left, +1,
                    LCP2_STRATEGY_SIMPLE_PARENTHESIS_JUMP);
                if (0 <= after.left && (text.charAt(after.left) == '('))
                    after.right = lookupCorrespondingParenthesis2(text, after.left, +1, LCP2_STRATEGY_DYNAMIC);
                else
                    after.right = -1;
            }
            System.err.println(
                String.format("leftPos=%d rightPos=%d posL=%d posR=%d",
                    before.left, before.right,
                    before.left, before.right));

            if (after.right < 0 || after.left < 0) {
                after.left = before.left + ((before.right - before.left) / 2);
                after.right = after.left;
            }

            return true;
        }
    }

    static int leftWordEdgePos(CharSequence text, int pos) {
        int p = lookup(text, LispWordSelectionTransformer.parenthesesSelector, pos, -1);
        if (0 <= p)
            return p + 1;
        else
            return -1;
    }

    static int rightWordEdgePos(CharSequence text, int pos) {
        int p = lookup(text, LispWordSelectionTransformer.parenthesesSelector, pos, +1);
        if (0 <= p)
            return p;
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
            if (Kawapad.DEBUG)
                Kawapad.logInfo("SideParenthesisSelector:" + before);

            if (0 < direction) {
                after.left = recursiveIndexOf(text, before.right + 1, +1, ')', '(');
                if (0 <= after.left)
                    after.right = lookupCorrespondingParenthesis2(text, after.left, +1, LCP2_STRATEGY_DYNAMIC);
                else
                    after.right = -1;
            } else {
                after.right = recursiveIndexOf(text, before.left - 1, -1, '(', ')');
                if (0 <= after.right)
                    after.left = lookupCorrespondingParenthesis2(text, after.right, -1, LCP2_STRATEGY_DYNAMIC);
                else
                    after.left = -1;
            }
            return true;
        }
    }

    static final CaretTransformer PARENTHESIS_SELECT_LEFT_TRANSFORMER = new SideParenthesisSelector(-1);
    static final CaretTransformer PARENTHESIS_SELECT_RIGHT_TRANSFORMER = new SideParenthesisSelector(+1);

    static abstract class LispWordSelectionTransformer extends CaretTransformer {
        static final CharSelector parenthesesSelector = new CharSelector() {
            @Override
            public boolean select(char ch) {
                return Character.isWhitespace(ch) ||
                       ch == '(' ||
                       ch == ')' ||
                       ch == '"' ||
                       false;
            }
        };
        static final CharSelector nagatedParenthesesSelector = new CharSelector() {
            @Override
            public boolean select(char ch) {
                return !(Character.isWhitespace(ch) ||
                         ch == '(' ||
                         ch == ')' ||
                         ch == '"' ||
                         false);
            }
        };
    }

    static final CaretTransformer LISPWORD_SELECT_CURRENT_TRANSFORMER = new SelectCurrentWordTransformer();

    static class SelectCurrentWordTransformer extends LispWordSelectionTransformer {
        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after) {
            after.right = lookup(text, parenthesesSelector, before.right + 1, +1) - 1;
            after.left = lookup(text, parenthesesSelector, before.left + 0, -1) + 1;
            return true;
        }
    }

    static final CaretTransformer LISPWORD_SELECT_RIGHT_TRANSFORMER = new LispWordSelectRightTransformer();

    static class LispWordSelectRightTransformer extends LispWordSelectionTransformer {
        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after) {
            int p;
            if (before.direction < 0) {
                CaretPos before1 = new CaretPos(before.left, before.left, -1);
                CaretPos after1 = new CaretPos(before1);
                LISPWORD_SELECT_CURRENT_TRANSFORMER.process(text, before1, after1);
                p = after1.right + 1;
                after1.direction = before.direction;
                if (after1.equals(before)) {
                    after.set(after1);
                    after.direction = +1;
                    return true;
                }
            } else {
                p = before.right + 1;
            }

            after.left = lookup(text, nagatedParenthesesSelector, p, +1);
            after.right = lookup(text, parenthesesSelector, after.left, +1) - 1;
            // after.direction = +1;
            return true;
        }
    }

    static final CaretTransformer LISPWORD_SELECT_LEFT_TRANSFORMER = new LispWordSelectLeftTransformer();

    static class LispWordSelectLeftTransformer extends LispWordSelectionTransformer {
        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after) {
            int p;
            if (before.direction < 0) {
                p = before.left - 1;
            } else {
                CaretPos before1 = new CaretPos(before.right, before.right, -1);
                CaretPos after1 = new CaretPos(before1);
                LISPWORD_SELECT_CURRENT_TRANSFORMER.process(text, before1, after1);
                p = after1.left - 1;

                after1.direction = before.direction;
                if (after1.equals(before)) {
                    after.set(after1);
                    after.direction = -1;
                    return true;
                }
            }

            after.right = lookup(text, nagatedParenthesesSelector, p, -1);
            after.left = lookup(text, parenthesesSelector, after.right, -1) + 1;
            // after.direction = -1;
            return true;
        }
    }

    static class SearchNextWordTransformer extends CaretTransformer {
        String word;
        boolean wordSearch;
        int direction;

        public SearchNextWordTransformer(String word, boolean wordSearch, int direction) {
            this.word = word;
            this.wordSearch = wordSearch;
            this.direction = direction;
        }

        private static final String ID = KawapadTemporarySearchHighlighter.GROUP_ID;

        @Override
        public boolean process(CharSequence text, CaretPos before, CaretPos after) {
            Pattern p = Pattern.compile(
                KawapadTemporarySearchHighlighter.searchStringToPattern(word, wordSearch));

            Matcher m = p.matcher(text);
            if (0 < direction) {
                // search forward.
                if (m.find(before.right + 1)) {
                    after.left = m.start(ID);
                    after.right = m.end(ID) - 1;
                    return true;
                } else {
                    // if not found, restart from the first position.
                    if (m.reset().find()) {
                        after.left = m.start(ID);
                        after.right = m.end(ID) - 1;
                        return true;
                    } else {
                        return false;
                    }
                }
            } else {
                // search backward.
                int s0 = -1;
                int e0 = -1;
                while (m.find() && m.end(ID) < before.left) {
                    s0 = m.start(ID);
                    e0 = m.end(ID);
                }

                // if found , return the result.
                if (0 <= s0 && 0 <= e0) {
                    after.left = s0;
                    after.right = e0 - 1;
                    return true;
                } else {
                    // if not found, restart the search to go to
                    // the last match string.
                    s0 = -1;
                    e0 = -1;
                    while (m.find()) {
                        s0 = m.start(ID);
                        e0 = m.end(ID);
                    }

                    // if found, then return it.
                    if (0 <= s0 && 0 <= e0) {
                        after.left = s0;
                        after.right = e0 - 1;
                        return true;
                    } else {
                        return false;
                    }
                }
            }
        }
    }

    static class JumpToCorrespondingParenthesisTransformer extends CaretTransformer {
        @Override
        protected boolean process(CharSequence text, CaretPos before, CaretPos after) {
            int beforeP = before.direction < 0 ? before.left : before.right;
            int afterP = SchemeParenthesisParser.lookupCorrespondingParenthesis(text, beforeP);
            if (afterP < 0) {
                return false;
            } else {
                after.left = afterP;
                after.right = afterP - 1;
                return true;
            }
        }
    }

    static final CaretTransformer JUMP_TO_CORRESPONDING_PARENTHESIS = new JumpToCorrespondingParenthesisTransformer();

    static class JumpAndSelectToCorrespondingParenthesisTransformer extends CaretTransformer {
        @Override
        protected boolean process(CharSequence text, CaretPos before, CaretPos after) {
            int beforeP = before.direction < 0 ? before.left : before.right;
            int afterP = SchemeParenthesisParser.lookupCorrespondingParenthesis(text, beforeP);
            if (afterP < 0) {
                return false;
            } else {
                if (beforeP < afterP) {
                    after.left = beforeP;
                    after.right = afterP;
                    after.direction = +1;
                } else {
                    after.left = afterP;
                    after.right = beforeP;
                    after.direction = -1;
                }
                return true;
            }
        }
    }

    static final CaretTransformer JUMP_AND_SELECT_TO_CORRESPONDING_PARENTHESIS = new JumpAndSelectToCorrespondingParenthesisTransformer();

    static void parenthesisSwapWords(Document document, Caret caret, int direction) {
        lispwordSwapWords(document, caret, direction, PARENTHESIS_SELECT_LEFT_TRANSFORMER,
            PARENTHESIS_SELECT_RIGHT_TRANSFORMER);
    }

    static void lispwordSwapWords(Document document, Caret caret, int direction) {
        lispwordSwapWords(document, caret, direction, LISPWORD_SELECT_LEFT_TRANSFORMER,
            LISPWORD_SELECT_RIGHT_TRANSFORMER);
    }

    static void lispwordSwapWords(Document document, Caret caret, int direction,
        CaretTransformer transLeft, CaretTransformer transRight) {
        Segment text = new Segment();
        try {
            document.getText(0, document.getLength(), text);
        } catch (BadLocationException e) {
            Kawapad.logError("", e);
        }
        CaretPos before = new CaretPos(caret);
        CaretPos after = new CaretPos(before);
        CaretTransformer transformer;
        if (direction < 0) {
            transformer = transLeft;
        } else {
            transformer = transRight;
        }
        transformer.process(text, before, after);

        CaretPos left;
        CaretPos right;
        if (direction < 0) {
            left = after;
            right = before;
        } else {
            left = before;
            right = after;
        }

        // 1... | before text | left word | middle text | right word | after text |
        // *** SWAP THESE WORDS ***
        // 2... | before text | right word | middle text | left word | after text |
        if (CaretTransformer.isValidCaretPos(text, after)) {
            try {
                String leftString = document.getText(left.left, left.right - left.left + 1);
                String rightString = document.getText(right.left, right.right - right.left + 1);
                document.remove(right.left, right.right - right.left + 1);
                document.insertString(right.left, leftString, null);
                Kawapad.logInfo(String.format("L:'%s' R:'%s'", leftString, rightString));
                document.remove(left.left, left.right - left.left + 1);
                document.insertString(left.left, rightString, null);

                CaretPos selectPos;
                if (direction < 0) {
                    selectPos = new CaretPos(left);
                    selectPos.right = selectPos.left + rightString.length() - 1;
                } else {
                    selectPos = new CaretPos(right);
                    int diff = (rightString.length() - leftString.length());
                    selectPos.left += diff;
                    selectPos.right = selectPos.left + leftString.length() - 1;
                }
                selectPos.setCaret(caret);

            } catch (BadLocationException e) {
                Kawapad.logError("", e);
            }
        }
    }

    // :( I think this is not what I want.
    // (Wed, 18 Sep 2019 14:54:39 +0900)
    static void lispwordJumpOverWords(Document document, Caret caret, int direction,
        CaretTransformer transLeft, CaretTransformer transRight) {
        Segment text = new Segment();
        try {
            document.getText(0, document.getLength(), text);
        } catch (BadLocationException e) {
            Kawapad.logError("", e);
        }
        CaretPos before = new CaretPos(caret);
        CaretPos after = new CaretPos(before);
        CaretTransformer transformer;
        if (direction < 0) {
            transformer = transLeft;
        } else {
            transformer = transRight;
        }
        transformer.process(text, before, after);

        CaretPos left;
        CaretPos right;
        if (direction < 0) {
            left = after;
            right = before;
        } else {
            left = before;
            right = after;
        }

        if (CaretTransformer.isValidCaretPos(text, after)) {
            try {
                String leftString = document.getText(left.left, left.right - left.left + 1);
                String rightString = document.getText(right.left, right.right - right.left + 1);

                // document.remove( right.left, right.right - right.left + 1);
                // document.insertString( right.left, leftString, null );
                // Kawapad.logInfo( String.format("L:'%s' R:'%s'" , leftString, rightString ));
                // document.remove( left.left, left.right - left.left + 1 );
                // document.insertString( left.left, rightString, null );

                CaretPos selectPos;
                if (direction < 0) {
                    document.remove(right.left, right.right - right.left + 2); // remove with the next space char.
                    document.insertString(left.left, rightString + " ", null);
                    selectPos = new CaretPos(
                        left.left,
                        left.left + rightString.length() - 1,
                        right.direction);
                } else {
                    int leftLen = left.right - left.left + 2; // remove with the next space char.
                    document.remove(left.left, leftLen);
                    document.insertString(right.right - leftLen + 2, leftString + " ", null);
                    selectPos = new CaretPos(
                        right.right - leftLen + 2,
                        right.right - leftLen + 2 + leftString.length() - 1,
                        left.direction);
                }

                selectPos.setCaret(caret);

            } catch (BadLocationException e) {
                Kawapad.logError("", e);
            }
        }
    }

    static void parenthesisExtendSelection(Document document, Caret caret, int direction) {
        extendSelection(document, caret, direction, true, PARENTHESIS_SELECT_LEFT_TRANSFORMER,
            PARENTHESIS_SELECT_RIGHT_TRANSFORMER);
    }

    static void extendSelection(Document document, Caret caret, int direction,
        boolean execSwapSelection, CaretTransformer transLeft, CaretTransformer transRight) {
        Segment text = new Segment();
        try {
            document.getText(0, document.getLength(), text);
        } catch (BadLocationException e) {
            Kawapad.logError("", e);
        }
        CaretPos before = new CaretPos(caret);
        CaretPos beforeOrg = new CaretPos(before);

        // This part is almost impossible to understand if you don't observe the
        // behavior of the cursor. Watch it at first. (Fri, 20 Sep 2019 19:33:23 +0900)
        if (execSwapSelection) {
            if (before.direction < 0) {
                int p = lookupCorrespondingParenthesis1(text, before.left);
                if (before.right == p && 0 < direction) {
                    // if the current selection is same as the current pair of parentheses,
                    // reverse the direction.
                    setCaretDirection(caret, +1);
                    return;
                } else {
                    before.right = p;
                }
            } else {
                int p = lookupCorrespondingParenthesis1(text, before.right);
                // if the current selection is same as the current pair of parentheses,
                // reverse the direction.
                if (before.left == p && direction < 0) {
                    setCaretDirection(caret, -1);
                    return;
                } else {
                    before.left = p;
                }
            }
        }

        CaretPos after = new CaretPos(before);
        CaretTransformer transformer;
        if (direction < 0) {
            transformer = transLeft;
        } else {
            transformer = transRight;
        }
        boolean result = transformer.process(text, before, after);

        if (result) {
            CaretPos total = new CaretPos(after);
            if (beforeOrg.direction < 0) {
                if (direction < 0) {
                    total.left = Math.min(after.left, before.left);
                    total.right = Math.max(after.right, beforeOrg.right);
                } else {
                    total.left = after.left;
                    total.right = Math.max(after.right, beforeOrg.right);
                }
            } else {
                if (direction < 0) {
                    total.left = Math.min(after.left, beforeOrg.left);
                    total.right = after.right;
                } else {
                    total.left = Math.min(after.left, beforeOrg.left);
                    total.right = Math.max(after.right, before.right);
                }
            }
            // total.direction = direction;
            total.setCaret(caret);
        } else {

        }
    }

    static void lispwordExtendSelection(Document document, Caret caret, int direction) {
        extendSelection(document, caret, direction, false, LISPWORD_SELECT_LEFT_TRANSFORMER,
            LISPWORD_SELECT_RIGHT_TRANSFORMER);
    }

    static void setCaretDirection(Caret caret, int direction) {
        int dot = caret.getDot();
        int mark = caret.getMark();
        if (direction < 0) {
            if (dot < mark) {
            } else {
                caret.setDot(dot);
                caret.moveDot(mark);
            }
        } else {
            if (dot < mark) {
                caret.setDot(dot);
                caret.moveDot(mark);
            } else {
            }
        }
    }
}
