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

import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.logging.Level;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter;
import javax.swing.text.Highlighter.HighlightPainter;

import lamu.lib.logging.Logger;

import javax.swing.text.JTextComponent;

public class KawapadTemporaryHighlighter {
    protected static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    protected static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    protected static void logInfo(String msg)               { LOGGER.log(Level.INFO, msg);      } 
    protected static void logWarn(String msg)               { LOGGER.log(Level.WARNING, msg);   }
    protected static final boolean DEBUG = false;
    
//  private static final boolean DEBUG = false;

    protected static class ClearingPosition {
        Highlighter highlighter;
        Object tag;
        public ClearingPosition(Highlighter highlighter, Object tag ) {
            super();
            this.highlighter = highlighter;
            this.tag = tag;
        }
        public void clear() {
            highlighter.removeHighlight( tag );
        }
    }

    protected static ArrayDeque<ClearingPosition> clearQueue = new ArrayDeque<>();
    protected static void addClearingHighlightQueue(Highlighter highlighter, Object tag ) {
        ((DefaultHighlighter)highlighter).setDrawsLayeredHighlights( true );
        synchronized ( clearQueue ) {
            ClearingPosition cp = new ClearingPosition( highlighter, tag );
            clearQueue.push(cp);
        }
    }
    protected static void eliminateClearingHighlightQueue() {
        synchronized ( clearQueue ) {
            for ( ClearingPosition cp : clearQueue ) {
                cp.clear();
            }
            clearQueue.clear();
        }
    }
    protected static void popClearingHighlightQueue() {
        synchronized ( clearQueue ) {
            if ( ! clearQueue.isEmpty() ) {
                clearQueue.pop().clear();
            }
        }
    }

    protected static void addHighlight( JTextComponent tc, int startPos, int endPos, HighlightPainter painter ) throws BadLocationException {
        addHighlight( tc.getHighlighter(), startPos, endPos, painter );
    }
    protected static void addHighlight( Highlighter highlighter, int startPos, int endPos, HighlightPainter painter ) throws BadLocationException {
        addClearingHighlightQueue( 
            highlighter, 
            highlighter.addHighlight( startPos, endPos, painter ) );
    }
    
    protected static void addParenthesisHighlight( JTextComponent tc, int open_pos, int close_pos, HighlightPainter painter ) throws BadLocationException {
        if ( DEBUG )
            logInfo( "highlightParentheses: " + open_pos + "/" + close_pos );
        
        Highlighter highlighter = tc.getHighlighter(); 
        addHighlight( highlighter, open_pos , open_pos +1, painter );
        addHighlight( highlighter, close_pos, close_pos+1, painter );
    }
    
}
