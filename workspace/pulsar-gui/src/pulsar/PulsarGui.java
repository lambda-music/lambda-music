package pulsar;

import java.awt.Dimension;
import java.awt.Point;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.Level;

import javax.swing.JSplitPane;

import gnu.mapping.Procedure;
import lamu.lib.evaluators.SchemeValues;
import lamu.lib.log.Logger;
import lamu.lib.scheme.proc.MultipleNamedProcedure1;
import lamu.lib.scheme.proc.MultipleNamedProcedureN;
import lamu.utils.lib.JNamedPanel;

public class PulsarGui {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }

    private final PulsarFrame frame;
    public PulsarGui(PulsarFrame frame) {
        super();
        this.frame = frame;
    }
    public PulsarFrame getCurrent() {
        return frame;
    }

    void init() {
    }

    public final Procedure setProgressPos = new SetProgressPos(new String[] { "gui-set-progress-pos" });
    public final Procedure clear = new Clear(new String[] { "gui-clear" });
    public final Procedure dividerLocation = new DividerLocation(new String[] { "gui-divider-location" });
    public final Procedure frameHeight = new FrameHeight(new String[] { "gui-frame-height" });
    public final Procedure frameWidth = new FrameWidth(new String[] { "gui-frame-width" });
    public final Procedure frameLeft = new FrameLeft(new String[] { "gui-frame-left" });
    public final Procedure frameTop = new FrameTop(new String[] { "gui-frame-top" });
    public final Procedure frameDividerPosition = new FrameDividerPosition(new String[] { "gui-frame-divider-position" });
    public final Procedure insertText = new InsertText(new String[] { "gui-insert-text" });
    public final Procedure setTempo = new SetTempo(new String[] { "gui-set-tempo" });
    

    public JNamedPanel getPane() {
        return getCurrent().userPane;
    }
    public PulsarFrame getFrame() {
        return getCurrent().frame;
    }
    public Procedure getSetProgressPos() {
        return setProgressPos;
    }
    public Procedure getClear() {
        return clear;
    }
    public Procedure getDividerLocation() {
        return dividerLocation;
    }
    public Procedure getFrameHeight() {
        return frameHeight;
    }
    public Procedure getFrameWidth() {
        return frameWidth;
    }
    public Procedure getFrameLeft() {
        return frameLeft;
    }
    public Procedure getFrameTop() {
        return frameTop;
    }
    public Procedure getFrameDividerPosition() {
        return frameDividerPosition;
    }
    public Procedure getInsertText() {
        return insertText;
    }
    public Procedure getSetTempo() {
        return setTempo;
    }

    public final class GetPane extends MultipleNamedProcedureN {
        public GetPane(String[] names) {
            super(names);
        }

        // TODO ???
        @Override
        public Object applyN(Object[] args) throws Throwable {
            logInfo("gui-get-pane");
            return getCurrent().userPane;
        }
    }

    public final class GetFrame extends MultipleNamedProcedureN {
        public GetFrame(String[] names) {
            super(names);
        }

        // TODO ???
        @Override
        public Object applyN(Object[] args) throws Throwable {
            logInfo("gui-get-frame");
            return getCurrent().frame;
        }
    }

    public final class SetProgressPos extends MultipleNamedProcedureN {
        public SetProgressPos(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            if ( 0 < args.length ) {
                double value = SchemeValues.toDouble(args[0]);
                getCurrent().pb_position.setValue((int) (value * PulsarFrame.PB_POSITION_MAX) );
            }
            return SchemeValues.NO_RESULT;
        }
    }

    public final class Clear extends MultipleNamedProcedureN {
        public Clear(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            getCurrent().guiClear();
            return SchemeValues.NO_RESULT;
        }
    }

    public final class DividerLocation extends MultipleNamedProcedureN {
        public DividerLocation(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            ArrayList<Object> argList = new ArrayList<Object>( Arrays.asList( args ) );
            if ( 1 == argList.size() ) {
                Object object = argList.get(0);
                if ( object instanceof JSplitPane ) {
                    JSplitPane pane = (JSplitPane) object;
                    return SchemeValues.toSchemeNumber( pane.getDividerLocation() );
                } else {
                    return SchemeValues.toSchemeNumber( -1 );
                }
            } else if ( 2 == argList.size() ) {
                Object object = argList.get(0);
                if ( object instanceof JSplitPane ) {
                    JSplitPane pane = (JSplitPane) object;
                    int location = SchemeValues.toInteger( argList.get(1) );
                    pane.setDividerLocation( location );
                    pane.revalidate();
                    return SchemeValues.toSchemeNumber( pane.getDividerLocation() );
                } else {
                    return SchemeValues.toSchemeNumber( -1 );
                }

            } else {
                throw new RuntimeException( 
                        "Invalid argument error\n"+
                        "usage : (gui-divider-location! [pane])" );
            }
        }
    }

    public final class FrameHeight extends MultipleNamedProcedureN {
        public FrameHeight(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            PulsarFrame pulsarGui = getCurrent();
            Dimension size = pulsarGui.frame.getSize();
            if ( 0 == args.length ) {
                
            } else {
                size.height = SchemeValues.toInteger(args[0]);
                pulsarGui.frame.setSize(size);
                pulsarGui.frame.revalidate();
            }
            return SchemeValues.toSchemeNumber( size.height );
        }
    }

    public final class FrameWidth extends MultipleNamedProcedureN {
        public FrameWidth(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            PulsarFrame pulsarGui = getCurrent();
            Dimension size = pulsarGui.frame.getSize();
            if ( 0 == args.length ) {
                
            } else {
                size.width = SchemeValues.toInteger(args[0]);
                pulsarGui.frame.setSize(size);
                pulsarGui.frame.revalidate();
            }
            return SchemeValues.toSchemeNumber( size.width );
        }
    }

    public final class FrameLeft extends MultipleNamedProcedureN {
        public FrameLeft(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            PulsarFrame pulsarGui = getCurrent();
            Point pos = pulsarGui.frame.getLocation();
            if ( 0 == args.length ) {
            } else {
                pos.x = SchemeValues.toInteger(args[0]);
                pulsarGui.frame.setLocation( pos);
                pulsarGui.frame.revalidate();
            }
            return SchemeValues.toSchemeNumber( pos.x );
        }
    }

    public final class FrameTop extends MultipleNamedProcedureN {
        public FrameTop(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            PulsarFrame pulsarGui = getCurrent();
            Point pos = pulsarGui.frame.getLocation();
            if ( 0 == args.length ) {
            } else {
                pos.y = SchemeValues.toInteger(args[0]);
                pulsarGui.frame.setLocation( pos);
                pulsarGui.frame.revalidate();
            }
            return SchemeValues.toSchemeNumber( pos.y );
        }
    }

    public final class FrameDividerPosition extends MultipleNamedProcedureN {
        public FrameDividerPosition(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            PulsarFrame pulsarGui = getCurrent();
            ArrayList<Object> argList = new ArrayList<Object>( Arrays.asList( args ) );
            if ( 0 == argList.size() ) {
                if ( pulsarGui.rootPane instanceof JSplitPane ) {
                    JSplitPane pane = (JSplitPane) pulsarGui.rootPane;
                    return SchemeValues.toSchemeNumber( pane.getDividerLocation() );
                } else {
                    return SchemeValues.toSchemeNumber( -1 );
                }
            } else if ( 1 == argList.size() ) {
                if ( pulsarGui.rootPane instanceof JSplitPane ) {
                    JSplitPane pane = (JSplitPane) pulsarGui.rootPane;
                    int location = SchemeValues.toInteger( argList.get(0) );
                    pane.setDividerLocation( location );
                    pulsarGui.frame.revalidate();
                    return SchemeValues.toSchemeNumber( pane.getDividerLocation() );
                } else {
                    return SchemeValues.toSchemeNumber( -1 );
                }
            } else {
                throw new RuntimeException( 
                        "Invalid argument error\n"+
                        "usage : (gui-panel-divider-position! [pane])" );
            }
        }
    }

    public final class InsertText extends MultipleNamedProcedureN {
        public InsertText(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            StringBuilder sb = new StringBuilder();
            for ( Object o : args ) {
                sb.append( o.toString() ).append( " " );
            }
            getCurrent().frame.getKawapad().insertText( sb.toString().trim() );
            return SchemeValues.NO_RESULT;
        }
    }

    public final class SetTempo extends MultipleNamedProcedure1 {
        public SetTempo(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg1) throws Throwable {
            getCurrent().setTempoDisplay( SchemeValues.toDouble( arg1 ) );
            return SchemeValues.NO_RESULT;
        }
    }

}
