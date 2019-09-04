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

import java.awt.Color;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;

import kawapad.SchemeParenthesisParser.ParserState;

public class KawaPadHighlighter2 {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    
//  private static final boolean DEBUG = false;

    static class ClearPosition {
        Highlighter highlighter;
        Object tag;
        public ClearPosition(Highlighter highlighter, Object tag ) {
            super();
            this.highlighter = highlighter;
            this.tag = tag;
        }
        public void clear() {
            highlighter.removeHighlight( tag );
        }
    }

    static ArrayDeque<ClearPosition> clearQueue = new ArrayDeque<>();
    static void addClearQueue(Highlighter highlighter, Object tag ) {
        synchronized ( clearQueue ) {
            ClearPosition cp = new ClearPosition( highlighter, tag );
            clearQueue.push(cp);
        }
    }
    static void eliminateClearQueue() {
        synchronized ( clearQueue ) {
            for ( ClearPosition cp : clearQueue ) {
                cp.clear();
            }
            clearQueue.clear();
        }
    }
    static void popClearQueue() {
        synchronized ( clearQueue ) {
            if ( ! clearQueue.isEmpty() ) {
                clearQueue.pop().clear();
            }
        }
    }
    
    static void highlightParentheses(JTextComponent tc, int open_pos, int close_pos) throws BadLocationException {
        Highlighter highlighter = tc.getHighlighter();
        {
            Object tag =  highlighter.addHighlight( open_pos, open_pos+1, new DefaultHighlighter.DefaultHighlightPainter( Color.GREEN ) );
            addClearQueue( highlighter, tag );
        }
        {
            Object tag =  highlighter.addHighlight( close_pos, close_pos+1, new DefaultHighlighter.DefaultHighlightPainter( Color.GREEN ) );
            addClearQueue( highlighter, tag );
        }
    }

    public static void forceClearHighlightedParenthesis() {
        eliminateClearQueue();
    }
    public static void clearHighlightedParenthesis() {
        popClearQueue();
    }

    public static void highlightMatchingParenthesis( JTextComponent component, int position ) throws BadLocationException {
        if ( Kawapad.ENABLED_SHOW_CORRESPONDING_PARENTHESES ) {
            String text = component.getText();
            ParserState parserState = 
                    SchemeParenthesisParser.lookupParenthesis( text, position );
            if ( parserState.isFound() )
                highlightParentheses( component, parserState.getIterator().getInitialIndex(), parserState.getIterator().getIndex() );
        }
    }
}
