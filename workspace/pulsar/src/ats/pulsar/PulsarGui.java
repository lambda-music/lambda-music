/*
 * Pulsar-Sequencer written by Atsushi Oka 
 * Copyright 2018 Atsushi Oka
 *
 * This file is part of Pulsar-Sequencer. 
 * 
 * Pulsar-Sequencer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Pulsar-Sequencer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with Pulsar-Sequencer.  If not, see <https://www.gnu.org/licenses/>.
 */

package ats.pulsar;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Dictionary;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.JTextComponent;

import org.jaudiolibs.jnajack.JackException;

import ats.kawapad.PulsarScratchPad;
import ats.pulsar.Pulsar.TempoTapperTempoNotifier;
import ats.pulsar.lib.swing.Action2;
import ats.pulsar.lib.swing.FlawLayout;
import ats.pulsar.lib.swing.JNamedPanel;
import ats.pulsar.lib.swing.JPulsarRadioButton;
import ats.pulsar.lib.swing.JPulsarUserObject;
import ats.pulsar.lib.swing.JSelectableUserObject;
import ats.pulsar.lib.swing.LayoutUtils;
import ats.pulsar.lib.swing.SchemeUtils;
import ats.pulsar.lib.swing.SpringLayoutUtil;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.Symbol;
import kawa.standard.Scheme;

class PulsarGui {
//	static void logError( String msg, Throwable e ) {
//        Logger.getLogger(Pulsar.class.getName()).log(Level.SEVERE, msg, e);
////		System.err.println( msg );
//	}
//	static void logInfo( String msg ) {
////        Logger.getLogger(Pulsar.class.getName()).log(Level.INFO, msg);
//		System.err.println( msg );
//	}
	static final Logger LOGGER = Logger.getLogger(PulsarGui.class.getName());
	static void logError(String msg, Throwable e) {
		LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo(String msg) {
		// LOGGER.log(Level.INFO, msg);
		System.err.println(msg);
	}
	static void logWarn(String msg) {
		LOGGER.log(Level.WARNING, msg);
	}

	static final int BORDER_SIZE = 10;
	static final PanelOrientation DEFAULT_PANEL_ORIENTATION = PanelOrientation.BOTTOM;
	

	Pulsar pulsar;
	PulsarGui( Pulsar pulsar ) {
		this.pulsar = pulsar;
		/**
		 * Initialize GUI 
		 */
        //Create and set up the window.
        this.frame = new JPulsarFrame();
	}

	enum TempoRange {
		WIDE  ("Wide",   0,1000), 
		NORMAL("Normal", 0,500), 
		SLOW  ("Slow",   0,100), 
		MEDIUM("Medium", 100,250),
		FAST  ("Fast",   250,500);
		private String caption;
		private int min;
		private int max;
		private TempoRange(String caption, int min, int max ) {
			this.caption = caption;
			this.min = min;
			this.max = max;
		}
		public final Action createAction( PulsarGui gui ) {
			return new AbstractAction() {
				@Override
				public void actionPerformed(ActionEvent e) {
					gui.guiSetTempoRange( TempoRange.this );
				}
				{
					putValue( Action2.NAME,  caption );
					putValue( Action.MNEMONIC_KEY, (int)caption.charAt(0) );
//					putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK) );
				}
			};
		};
	}
    public static BorderLayout newLayout() {
    	return new BorderLayout( BORDER_SIZE, BORDER_SIZE );
    }
	
	enum PanelOrientation {
		TOP   ( "Top",    JSplitPane.VERTICAL_SPLIT   ),
		RIGHT ( "Right",  JSplitPane.HORIZONTAL_SPLIT ), 
		BOTTOM( "Bottom", JSplitPane.VERTICAL_SPLIT   ),
		LEFT  ( "Left",   JSplitPane.HORIZONTAL_SPLIT ), 
		;
		final String caption;
		final int orientation;
		private PanelOrientation( String caption, int orientation ) {
			this.caption = caption;
			this.orientation = orientation;
		}
		public final Action createSetPanelOrientationAction( PulsarGui gui ) {
			return new AbstractAction() {
				@Override
				public void actionPerformed(ActionEvent e) {
					gui.guiSetPanelOrientation( PanelOrientation.this );
				}
				{
					putValue( Action2.NAME, caption );
					putValue( Action.MNEMONIC_KEY, (int)caption.charAt(0) );
//					putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK) );
				}
			};
		};
		public final Action createSetScratchpadOrientationAction( PulsarGui gui ) {
			return new AbstractAction() {
				@Override
				public void actionPerformed(ActionEvent e) {
					gui.frame.guiSetScratchpadOrientation( PanelOrientation.this );
				}
				{
					putValue( Action2.NAME, caption );
					putValue( Action.MNEMONIC_KEY, (int)caption.charAt(0) );
//					putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK) );
				}
			};
		};
	}
	
	
    void initScheme(Scheme scheme) {
    	//////////////////////////////////////////////////////
    	
    	SchemeUtils.defineVar( scheme, "gui-get-pane" , new ProcedureN() {
			// TODO ???
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("gui-get-pane");
    			return userPane;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-get-frame" , new ProcedureN() {
			// TODO ???
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("gui-get-frame");
    			return frame;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-clear!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiClear();
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-pack!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiPack();
    			return EmptyList.emptyList;
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-new" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
//				try {
//					return SchemeNewFactory.process( pulsar, args);
//				} catch ( Exception e ) {
//					logError("", e);
//					return null;
//				}
				return SchemeNewFactory.process( pulsar, args);
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-parent" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 1 <= args.length ) {
        			ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
        			Component component = (Component) SchemeUtils.schemeNullToJavaNull( argList.pop() );
    				Container parent = component.getParent();
					return SchemeUtils.javaNullCheck( parent );
    			} else {
					throw new RuntimeException( 
							"Invalid argument error\n"+
							"usage : (gui-remove-by-name [parent] [name])" );
    			}
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-remove-all" , new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 1 <= args.length ) {
        			ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
        			Container parent = (Container) SchemeUtils.schemeNullToJavaNull( argList.pop() );
        			guiRemoveAll(parent);
        			return EmptyList.emptyList;
    			} else {
					throw new RuntimeException( 
							"Invalid argument error\n"+
							"usage : (gui-remove-by-name [parent] [name])" );
    			}
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-remove-by-ref" , new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 1 < args.length ) {
        			ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
        			Container parent = (Container) SchemeUtils.schemeNullToJavaNull( argList.pop() );
        			guiRemoveByRef( parent, argList );
        			return EmptyList.emptyList;
    			} else {
					throw new RuntimeException( 
							"Invalid argument error\n"+
							"usage : (gui-remove-by-name [parent] [name])" );
    			}
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-remove-by-name" , new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 1 < args.length ) {
        			ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
        			Container parent = (Container) SchemeUtils.schemeNullToJavaNull( argList.pop() );
        			Collection<String> path = SchemeUtils.convertList(argList, (o)->{
    					return SchemeUtils.toString(o);
    				});
    				Component c  = guiRemoveByPath( parent, path );
    				return SchemeUtils.javaNullToSchemeNull( c );
    			} else {
					throw new RuntimeException( 
							"Invalid argument error\n"+
							"usage : (gui-remove-by-name [parent] [name])" );
    			}
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-invoke-later" , new ProcedureN() {
			@Override
			public Object applyN( Object[] args ) throws Throwable {
				guiInvokeLater( (Procedure) args[0], Arrays.copyOfRange(args, 1, args.length ) );
				return true;
			}
    	});

    	
    	SchemeUtils.defineVar( scheme, "gui-get" , new ProcedureN() {
    		@Override
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 1 == args.length ) {
        			Container parent = (Container) args[0];
        			Collection<Entry<String, Component>> list =  guiListAll(parent);
    				SchemeUtils.<Entry<String, Component>, Pair>convertList(list, (e)->{
    					String key = e.getKey();
    					return Pair.make(
    							( key == null ? false : SchemeUtils.toSchemeSymbol( key ) ) , e.getValue() );
    				});
					return Pair.makeList( (List)list );
    			} else if ( 1 < args.length ) {
        			ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
        			Container parent = (Container) SchemeUtils.schemeNullToJavaNull( argList.pop() );
        			Collection<String> path = SchemeUtils.convertList(argList, (o)->{
    					return SchemeUtils.toString(o);
    				});
        			
    				Component c  = guiGet( parent, path );
    				return SchemeUtils.javaNullToSchemeNull( c );
    			} else {
    				throw new RuntimeException( 
    						"Invalid argument error\n"+
    						"usage : (gui-get [parent] [name ... ])" );
    			}
    		}
    	});
    	
    	
    	SchemeUtils.defineVar( scheme, "gui-set-selected" , new ProcedureN() {
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 3 == args.length ) {
        			JSelectableUserObject parent = (JSelectableUserObject) args[0];
        			Object userObject = args[1];
        			boolean selected  = (Boolean)args[2];
        			
					int count = parent.setSelectedByUserObject(userObject, selected);
					return SchemeUtils.toSchemeNumber( count ); 
    			} else {
    				throw new RuntimeException( 
    						"Invalid argument error\n"+
    						"usage : (gui-set-selected [selectable object] [selected value])" );
    			}
    			
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-set-text" , new ProcedureN() {
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 2 == args.length ) {
    				JTextComponent parent = (JTextComponent) args[0];
        			String userText = SchemeUtils.anyToString( args[1] );
					parent.setText( userText );
					return parent; 
    			} else {
    				throw new RuntimeException( 
    						"Invalid argument error\n"+
    						"usage : (gui-set-text text-component text )" );
    			}
    			
    		}
    	});

    	
    	/**
    	 * (gui-get-user-object (gui-get (part1 'gui) 'self) )
    	 * 
    	 */
    	SchemeUtils.defineVar( scheme, "gui-set-user-object" , new ProcedureN() {
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 2 == args.length ) {
    				JPulsarUserObject parent = (JPulsarUserObject) args[0];
        			Object userObject = args[1];
					parent.setUserObject( userObject );
					return parent; 
    			} else {
    				throw new RuntimeException( 
    						"Invalid argument error\n"+
    						"usage : (gui-set-user-object parent user-object )" );
    			}
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-get-user-object" , new ProcedureN() {
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 1 == args.length ) {
    				JPulsarUserObject parent = (JPulsarUserObject) args[0];
					return parent.getUserObject();
    			} else {
    				throw new RuntimeException( 
    						"Invalid argument error\n"+
    						"usage : (gui-get-user-object parent)" );
    			}
    		}
    	});
    
    	// XXX
    	SchemeUtils.defineVar( scheme, "gui-prettify" , new ProcedureN() {
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 1 == args.length ) {
        			String userText = SchemeUtils.anyToString( args[0] );
					return frame.prettify( userText );
    			} else {
    				throw new RuntimeException( 
    						"Invalid argument error\n"+
    						"usage : (gui-set-text text-component text )" );
    			}
    			
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-divider-location!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				ArrayList<Object> argList = new ArrayList<Object>( Arrays.asList( args ) );
				if ( 1 == argList.size() ) {
					Object object = argList.get(0);
					if ( object instanceof JSplitPane ) {
						JSplitPane pane = (JSplitPane) object;
						return SchemeUtils.toSchemeNumber( pane.getDividerLocation() );
					} else {
						return SchemeUtils.toSchemeNumber( -1 );
					}
				} else if ( 2 == argList.size() ) {
					Object object = argList.get(0);
					if ( object instanceof JSplitPane ) {
						JSplitPane pane = (JSplitPane) object;
						int location = SchemeUtils.toInteger( argList.get(1) );
						pane.setDividerLocation( location );
						pane.revalidate();
						return SchemeUtils.toSchemeNumber( pane.getDividerLocation() );
					} else {
						return SchemeUtils.toSchemeNumber( -1 );
					}

				} else {
    				throw new RuntimeException( 
    						"Invalid argument error\n"+
    						"usage : (gui-divider-location! [pane])" );
				}
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-frame-height!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				Dimension size = frame.getSize();
				if ( 0 == args.length ) {
					
				} else {
					size.height = SchemeUtils.toInteger(args[0]);
					frame.setSize(size);
					frame.revalidate();
				}
				return SchemeUtils.toSchemeNumber( size.height );
			}
    		
    	});
    	SchemeUtils.defineVar( scheme, "gui-frame-width!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				Dimension size = frame.getSize();
				if ( 0 == args.length ) {
					
				} else {
					size.width = SchemeUtils.toInteger(args[0]);
					frame.setSize(size);
					frame.revalidate();
				}
				return SchemeUtils.toSchemeNumber( size.width );
			}
    		
    	});
    	SchemeUtils.defineVar( scheme, "gui-frame-left!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				Point pos = frame.getLocation();
				if ( 0 == args.length ) {
				} else {
					pos.x = SchemeUtils.toInteger(args[0]);
					frame.setLocation( pos);
					frame.revalidate();
				}
				return SchemeUtils.toSchemeNumber( pos.x );
			}
    		
    	});
    	SchemeUtils.defineVar( scheme, "gui-frame-top!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				Point pos = frame.getLocation();
				if ( 0 == args.length ) {
				} else {
					pos.y = SchemeUtils.toInteger(args[0]);
					frame.setLocation( pos);
					frame.revalidate();
				}
				return SchemeUtils.toSchemeNumber( pos.y );
			}
    		
    	});

    	SchemeUtils.defineVar( scheme, "gui-frame-divider-position!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				ArrayList<Object> argList = new ArrayList<Object>( Arrays.asList( args ) );
				if ( 0 == argList.size() ) {
					if ( rootPane instanceof JSplitPane ) {
						JSplitPane pane = (JSplitPane) rootPane;
						return SchemeUtils.toSchemeNumber( pane.getDividerLocation() );
					} else {
						return SchemeUtils.toSchemeNumber( -1 );
					}
				} else if ( 1 == argList.size() ) {
					if ( rootPane instanceof JSplitPane ) {
						JSplitPane pane = (JSplitPane) rootPane;
						int location = SchemeUtils.toInteger( argList.get(0) );
						pane.setDividerLocation( location );
						frame.revalidate();
						return SchemeUtils.toSchemeNumber( pane.getDividerLocation() );
					} else {
						return SchemeUtils.toSchemeNumber( -1 );
					}
				} else {
    				throw new RuntimeException( 
    						"Invalid argument error\n"+
    						"usage : (gui-panel-divider-position! [pane])" );
				}
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-frame-orientation!" , new ProcedureN() {
    		PulsarGui.PanelOrientation sym2orientation( Object sym ) {
    			String id = SchemeUtils.symbolToString( sym );
    			return PanelOrientation.valueOf(id.toUpperCase() );
    		}
    		Symbol orientation2sym( PulsarGui.PanelOrientation orientation ) {
    			return SchemeUtils.toSchemeSymbol( orientation.name().toLowerCase() );
    		}
    		
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				ArrayList<Object> argList = new ArrayList<Object>( Arrays.asList( args ) );
				if ( 0 == argList.size() ) {
					return orientation2sym( currentPanelOrientation );
				} else if ( 1 == argList.size() ) {
					guiSetPanelOrientation( sym2orientation( argList.get(0) ) );
					return argList.get(0);
				} else {
    				throw new RuntimeException( 
    						"Invalid argument error\n"+
    						"usage : (gui-panel-orientation! [pane])" );
				}
    		}
    	});

    	
    	SchemeUtils.defineVar( scheme, "gui-build!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				return guiBuild(args);
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-newline!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("newline-gui");
				if ( args.length == 0 )
					guiNewline(null);
				else
					guiNewline((JComponent) args[0]);
				
    			return EmptyList.emptyList;
    		}
    	});
//    	SchemeUtils.defineVar( scheme, "gui-repaint" , new ProcedureN() {
//			@Override
//    		public Object applyN(Object[] args) throws Throwable {
////					logInfo("refresh-gui");
//				guiRepaint();
//    			return EmptyList.emptyList;
//    		}
//    	});
    	SchemeUtils.defineVar( scheme, "gui-layout!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-layout" );
				
				if ( args.length == 0 ) {
					throw new IllegalArgumentException();
				} else if ( args.length <= 1 ) {
					guiLayout( userPane, SchemeUtils.symbolToString( args[0] ) );
					return userPane;
				} else {
					ArrayList<Object> argList = new ArrayList<>( Arrays.asList(args) );
					Container container = 0 < argList.size() ? (Container) argList.remove(0) : userPane;
					String    type      = 0 < argList.size() ? SchemeUtils.symbolToString( argList.remove(0) ) : "default";
					if ( container instanceof JFrame ){
						container = ((JFrame)container).getContentPane();
					}
					guiLayout( container, type, argList.toArray() );

					return container;
				}
    		}
    	});
    	
    	SchemeUtils.defineVar( scheme, "gui-gridbag-layout" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-gridbag-layout" );
				guiGridBagLayout(userPane);
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-spring-layout" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-spring-layout" );
				guiSpringLayout(userPane);
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-flow-layout" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-flow-layout" );
				guiFlowLayout(userPane);
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-put-constraint" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo("refresh-gui");
			 	SpringLayout springLayout = ((SpringLayout)userPane.getLayout());
				if ( args.length == 5 ) {
					new SpringLayoutUtil(springLayout, userPane).putConstraint( args[0],args[1],args[2],args[3],args[4]  );
				} else if ( args.length == 4 ) {
					new SpringLayoutUtil(springLayout, userPane).putConstraint( args[0],args[1], 5,     args[2], args[3] );
				} else {
					throw new RuntimeException( "put-constraint has five parameters( constraint1 component1 pad constraint2 component2  )." );
				}
				guiRepaint(userPane);
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-invalidate" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiInvalidate((Container) args[0] );
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-validate" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiValidate((Container) args[0] );
    			return EmptyList.emptyList;
    		}
    	});
    	
    	SchemeUtils.defineVar( scheme, "gui-revalidate" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiRevalidate((Container) args[0] );
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-repaint" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				guiRepaint((Container) args[0] );
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-insert-text!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				StringBuilder sb = new StringBuilder();
				for ( Object o : args ) {
					sb.append( o.toString() ).append( " " );
				}
				frame.insertText( sb.toString().trim() );
    			return EmptyList.emptyList;
    		}
    	});
    	
    	PulsarScratchPad.initScheme( scheme );
    }
	
	//Create the "cards".
    JPulsarFrame frame;
    JComponent rootPane; 
	JPanel staticPane;
	JNamedPanel userPane ;
	JPanel staticPaneOuter;
	JScrollPane userPaneOuter;

    transient boolean isComboBoxUpdating = false;
	JComboBox<String> cb_relatedFiles;
	JTextField tf_currentFile;
	JProgressBar pb_position;
	JSlider sl_tempoSlider;
	
	public void guiClear() {
		userPane.removeAll();
		guiFlowLayout(userPane);
		frame.invalidate();
		frame.revalidate();
		// frame.pack();
	}
	public void guiInvokeLater( Procedure procedure, Object ... args  ) {
		SwingUtilities.invokeLater( pulsar.createRunnableAndInvocable( procedure, args ) );
	}
	public void guiPack() {
		//
		// (Sat, 29 Sep 2018 23:17:02 +0900) TAG_PACK_TWICE
		//
		// Pack twice. This is a countermeasure for a bug of JScrollPane that it does
		// not exclude the width/height of its scroll-bars from
		// internal content pane width/height.
		// 
		// scrollPane.setHorizontalScrollBarPolicy( JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );
		// scrollPane.setVerticalScrollBarPolicy(J ScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED );
		// 
		// Even if set HORIZONTAL_SCROLLBAR_AS_NEEDED / VERTICAL_SCROLLBAR_AS_NEEDED is
		// set, the scroll-bar always appears after pack() method is called.
		//
		// Because it resizes the components without considering the width/height that
		// will occupied by the scroll-bars themselves, the scroll-bars always appear
		// even if it is not necessary.
		// 
		// A simple Countermeasure is just packing twice.
		//
		
//		frame.pack();
//		frame.pack();
		
		// SOMEHOW THIS MAKES THE FRAME SMALLER AND SMALLER WHEN DYNAMICALLY CREATING GROUPS.
		// SO I DISABLED IT. (Mon, 15 Jul 2019 10:08:05 +0900)
		// frame.pack();
		// frame.pack();
	}
	
	public void guiFlowLayout(Container userPane) {
		userPane.setLayout( new FlawLayout( FlawLayout.LEFT, 2, 2 ) );
	}
	public void guiBorderLayout(Container userPane) {
		userPane.setLayout( new BorderLayout( BORDER_SIZE,BORDER_SIZE ) );
	}
	public void guiSpringLayout( Container userPane ) {
		userPane.setLayout( new SpringLayout() );
	}
	public void guiBoxLayout( Container userPane, int axis ) {
		userPane.setLayout( new BoxLayout( userPane, axis ) );
	}
	public void guiGridBagLayout(Container userPane) {
		userPane.setLayout( new GridBagLayout() );
	}
	public void guiGridLayout( Container userPane, int row, int col, int hgap, int vgap  ) {
		logInfo( "gridLayout : " + row + " / " + col + " / " + hgap + " / " + vgap );
		userPane.setLayout( new GridLayout( row, col, hgap, vgap ) );
	}

	//
	public void guiLayout_auto( Container container, Object[] args ) {
		ArrayList<Object> argList = new ArrayList<>( Arrays.asList(args) );
		String    type = 0 < argList.size() ? SchemeUtils.symbolToString( argList.remove(0) ) : "default";
		guiLayout( container, type, argList.toArray() );
	}


	public void guiLayout( Container container, String type, Object ... args  ) {
		// logInfo( "guiLayout : container=" + container );
		switch ( type ) {
			case "default":
			case "flow" :
				guiFlowLayout( container );
				break;
			case "border" :
				guiBorderLayout( container );
				break;
			case "box" :
				guiBoxLayout( container,
						0 < args.length ? ((Number)args[0]).intValue() : BoxLayout.PAGE_AXIS 
								);
				break;
			case "spring" :
				guiSpringLayout( container );
				break;
			case "grid" :
				if ( args.length < 4 )
					throw new IllegalArgumentException("arguments must to be equal or more than four.");
				
				
				guiGridLayout( container,
						// 1 (Mon, 15 Jul 2019 10:05:46 +0900)
						// 2 (Tue, 16 Jul 2019 13:35:24 +0900) 
						( 0 < args.length ? ((Number)args[0]).intValue() : 0) , 
						( 1 < args.length ? ((Number)args[1]).intValue() : 0) , 
						( 2 < args.length ? ((Number)args[2]).intValue() : 0) , 
						( 3 < args.length ? ((Number)args[3]).intValue() : 0)  
					);
				break;
			case "gridbag" :
				guiGridBagLayout( container );
				break;
			default :
				throw new RuntimeException( "Unknown LayoutManager name : " + type );
		}
	}
//	public void guiRepaint() {
//		userPane.revalidate();
//		userPane.repaint();
//        // frame.pack();
//	}
	public void guiSetTempoRange( PulsarGui.TempoRange tempoRange ) {
		this.sl_tempoSlider.setMaximum( tempoRange.max );
		this.sl_tempoSlider.setMinimum( tempoRange.min );
	}
	
	HashMap<PulsarGui.PanelOrientation,AbstractButton> orientaionMap = new HashMap<>();
	PulsarGui.PanelOrientation currentPanelOrientation = PanelOrientation.BOTTOM;

	public void guiSetPanelOrientation( PulsarGui.PanelOrientation panelOrientation ) {

		if ( panelOrientation.equals(currentPanelOrientation ) )
			return;
		
		Dimension staticPaneSize = this.staticPane.getSize();
//			Dimension staticPaneSize = new Dimension( 600, 300 );
		Dimension userPaneSize   = this.userPane.getSize();
		 Dimension frameSize      = frame.getSize();
		int dividerLocation;
		Dimension newFrameSize = new Dimension( frameSize );
		
//			JSplitPane pulsarRootPane = (JSplitPane) this.rootPane;
		
		if ( rootPane instanceof JSplitPane )
			((JSplitPane)rootPane).setOrientation( panelOrientation.orientation );
		
		JComponent topComponent;
		JComponent bottomComponent;
		String borderDirection;
		
		switch ( panelOrientation ) {
			case TOP :
				newFrameSize.width  = Math.max( staticPaneSize.width, userPaneSize.width); 
				newFrameSize.height = staticPaneSize.height + userPaneSize.height; 
				dividerLocation     = userPaneSize.height;
				topComponent        = userPane;
				bottomComponent     = staticPane;
				borderDirection = BorderLayout.PAGE_END;
				break;
			case RIGHT :
				newFrameSize.width  = staticPaneSize.width + userPaneSize.width;
				newFrameSize.height = Math.max( staticPaneSize.height, userPaneSize.height);
				dividerLocation     = staticPaneSize.width;
				topComponent        = staticPane;
				bottomComponent     = userPane;
				borderDirection     = BorderLayout.LINE_START;
				break;
			case BOTTOM :
				newFrameSize.width  = Math.max( staticPaneSize.width, userPaneSize.width); 
				newFrameSize.height = staticPaneSize.height + userPaneSize.height; 
				dividerLocation     = staticPaneSize.height;
				topComponent        = staticPane;
				bottomComponent     = userPane;
				borderDirection = BorderLayout.PAGE_START;
				break;
			case LEFT :
				newFrameSize.width  = staticPaneSize.width + userPaneSize.width;
				newFrameSize.height = Math.max( staticPaneSize.height, userPaneSize.height);
				dividerLocation     = userPaneSize.width;
				topComponent        = userPane;
				bottomComponent     = staticPane;
				borderDirection     = BorderLayout.LINE_END;
				break;
			default :
				throw new RuntimeException( "internal error" );
		}

		if ( rootPane instanceof JSplitPane ) {
			JSplitPane rootPane = (JSplitPane) this.rootPane;
			rootPane.setTopComponent(null);
			rootPane.setBottomComponent(null);
			rootPane.setTopComponent(topComponent);
			rootPane.setBottomComponent(bottomComponent);
		} else if ( rootPane instanceof JPanel ) {
			rootPane.remove( userPaneOuter );
			rootPane.add( userPaneOuter, BorderLayout.CENTER );
			rootPane.remove( staticPaneOuter );
			rootPane.add( staticPaneOuter, borderDirection );
			rootPane.revalidate();
		}
	
		newFrameSize.width += 20;
		newFrameSize.height += 20;
		frame.setSize( newFrameSize );
		
		if ( rootPane instanceof JSplitPane ) {
			JSplitPane rootPane = (JSplitPane) this.rootPane;
			rootPane.setDividerLocation(dividerLocation);
		}
		
		rootPane.revalidate();
		frame.revalidate();
		rootPane.validate();
		frame.validate();
		
		this.currentPanelOrientation = panelOrientation;

		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				orientaionMap.get(panelOrientation ).setSelected(true);
				if ( rootPane instanceof JSplitPane )
				((JSplitPane) rootPane).setDividerLocation(dividerLocation);
			}
		});
		
	}
	
	
	public Component guiResolve( Container parent, Collection<String> path, boolean errorIfNotFound ) {
		if ( parent == null )
			parent = userPane;
		
		Component curr = parent;
		
		List<String> tracePath = new ArrayList<>();
		for ( Iterator<String> i=path.iterator(); i.hasNext(); ) {
			String name = i.next();
			tracePath.add( name );

			if ( curr instanceof JNamedPanel ) {
				Component c = ((JNamedPanel)curr).getComponentByName( name );
				if ( c== null ) {
					if ( errorIfNotFound )
						throw new RuntimeException( name + " is not found : ( " + tracePath.toString() + " )" );
					else 
						return null;
				} else {
					curr = c;
				}
			} else {
				if ( errorIfNotFound )
					throw new RuntimeException( "The passed object is not a named container object." );
				else
					return null;
			}
		}
		return curr;
		
	}
	private void guiRemoveByRef(Container parent, Collection<Object> argList ) {
		for ( Object o : argList ) {
			parent.remove( (Component)o );
		}
	}
	private void guiRemoveByRef(Container parent, Component component) {
		parent.remove( component );
	}
	private void guiRemoveAll(Container parent ) {
		parent.removeAll();
	}

	public Component guiRemoveByPath( Container parent, Collection<String> path ) {
		Component c = guiResolve( parent, path, false );
		parent.remove( c );
		return c;
	}
	
	public Component guiGet( Container parent, Collection<String> path ) {
		return guiResolve( parent, path,  false );
	}
	public Collection<Entry<String, Component>> guiListAll( Container parent ) {
		ArrayList<Entry<String, Component>> list = new ArrayList<>();
		if ( parent instanceof JNamedPanel  ) {
			for ( Iterator<Entry<String, Component>> iterator = ((JNamedPanel)parent).listAllComponent();
					iterator.hasNext(); ) 
			{
				list.add( iterator.next() );
			}
		} else {
			throw new RuntimeException( "this panel is not made by pulsar" );
		}
		return list;
	}

	public void guiAdd( Container parent, Component c, Object constraint ) {
		if ( parent == null )
			parent = userPane;
		parent.add( c, constraint );
	}
	public void guiAdd( Container parent, Component c  ) {
		if ( parent == null )
			parent = userPane;
		parent.add( c  );
	}

	public Object guiBuild(Object[] args) {
		return guiBuild2(args);
	}

	@SuppressWarnings("unused")
	private Object guiBuild1(Object[] args) {
		Container parent;
		if ( args[0] instanceof Container )
			parent = (Container) args[0];
		else if ( args[0] instanceof EmptyList ) { 
			parent = null;
		} else {
			throw new RuntimeException( "An invalid parent object was specified. " );
		}
		
		String mode = null;
		for ( int i=1; i<args.length; i++ ) {
			Object curr = args[i];
			if ( curr instanceof Symbol ) {
				String symbolName = SchemeUtils.symbolToString( curr );

				if ( "name".equals( mode ) ) {
					guiName( parent, symbolName );
					mode = null;
				} else {
					switch ( symbolName ) {
						case "newline" : 
							guiNewline( parent );
							break;
						case "validate" : 
							guiValidate( parent );
							break;
						case "list":
							mode = "list";
							break;
						case "name":
							mode = "name";
							break;
						case "constraint":
							mode = "constraint";
							break;
						case "label":
							mode = "label";
							break;
						default :
							throw new RuntimeException( "gui-build! unknown type \"" + symbolName + "\"" );
					}
				}
			} else if ( curr instanceof IString ) {
				if ( "label".equals( mode ) ) {
					guiAdd( parent, (Component) SchemeNewFactory.process(
							pulsar,
							Symbol.makeUninterned("label"), 
							SchemeUtils.anyToString( curr ) ) );
					
					mode = null;
				} else if ( "name".equals( mode ) ) {
					guiName( parent, SchemeUtils.toString( curr ) ); 
					mode = null;
				} else if ( "constraint".equals( mode ) ) {
					guiConstraint( parent,  curr ); 
					mode = null;
				} else {
					throw new RuntimeException( "An invalid state was detected " + mode );
				}
			} else {
				if ( curr instanceof Pair  ) {
					Pair p = (Pair) curr;
					if ( "list".equals( mode ) ) {
						for ( Object e : p ) {
							guiAdd( parent, (Component) e );
						}
					} else {
						Object car = p.getCar();
						Object cdr = p.getCdr();
						
						if ( cdr instanceof Pair ) {
							guiAdd( parent, (Component)car, LayoutUtils.map2constraint( cdr ) );
						} else {
							guiAdd( parent, (Component)car, SchemeUtils.toString( cdr ) );
						}
					}
				} else {
					guiAdd( parent, (Component)curr );
				}
				mode = null;
			}
		}
		return parent;
	}	
	

	private Object guiBuild2(Object[] args) {
		Container parent;
		if ( args[0] instanceof Container )
			parent = (Container) args[0];
		else if ( args[0] instanceof EmptyList ) { 
			parent = null;
		} else {
			throw new RuntimeException( "An invalid parent object was specified. " );
		}
		
		String mode = null;
		for ( int i=1; i<args.length; i++ ) {
			try {
				Object curr = args[i];
	
				if ( "label".equals( mode ) ) {
					guiAdd( parent, 
							(Component)SchemeNewFactory.process(
									pulsar, 
									Symbol.makeUninterned("label"), 
									SchemeUtils.anyToString( curr )));
					mode = null;
				} else if ( "name".equals( mode ) ) {
					guiName( parent, SchemeUtils.toString( curr ) ); 
					mode = null;
				} else if ( "constraint".equals( mode ) ) {
					guiConstraint( parent,  curr ); 
					mode = null;
				} else if ( "property".equals( mode ) ) {
 					guiProperty( parent, (List<Object>) curr );
					mode = null;
				} else if ( "invokable".equals( mode ) ) {
 					guiNextProcedure( parent, (Procedure) curr );
					mode = null;
				} else if ( "index".equals( mode ) ) {
 					guiNextIndex( parent, SchemeUtils.toInteger( curr ) );
					mode = null;
				} else if ( "index-from-last".equals( mode ) ) {
 					guiNextIndex( parent, parent.getComponentCount() - SchemeUtils.toInteger( curr )  );
					mode = null;
				} else if ( "remove".equals( mode ) ) {
 					guiRemoveByRef( parent, (Component)curr);
					mode = null;
				} else {
					if ( curr instanceof Symbol ) {
						String symbolName = SchemeUtils.symbolToString( curr );
						switch ( symbolName ) {
							case "newline" : 
								guiNewline( parent );
								break;
							case "validate" : 
								guiValidate( parent );
								break;
							case "invalidate" : 
								guiInvalidate( parent );
								break;
							case "revalidate" : 
								guiRevalidate( parent );
								break;
							case "repaint" : 
								guiRepaint( parent );
								break;
							case "pack" : 
								guiPack();
								break;
							case "list":
								mode = "list";
								break;
							case "name":
								mode = "name";
								break;
							case "constraint":
								mode = "constraint";
								break;
							case "label":
								mode = "label";
								break;
							case "property":
								mode = "property";
								break;
							case "invokable":
								mode = "invokable";
								break;
							case "index":
								mode = "index";
								break;
							case "index-from-last":
								mode = "index-from-last";
								break;
							case "remove":
								mode = "remove";
								break;
							case "remove-all" : 
								guiRemoveAll( parent );
								break;
							default :
								throw new RuntimeException( "gui-build! unknown type \"" + symbolName + "\"" );
						}
					} else {
	//					if ( curr instanceof Pair  ) {
	//						Pair p = (Pair) curr;
	//						if ( "list".equals( mode ) ) {
	//							for ( Object e : p ) {
	//								guiAdd( parent, (Component) e );
	//							}
	//						} else {
	//							Object car = p.getCar();
	//							Object cdr = p.getCdr();
	//							
	//							if ( cdr instanceof Pair ) {
	//								guiAdd( parent, (Component)car, LayoutUtils.map2constraint( cdr ) );
	//							} else {
	//								guiAdd( parent, (Component)car, SchemeUtils.toString( cdr ) );
	//							}
	//						}
	//					} else {
	//						guiAdd( parent, (Component)curr );
	//					}
						// MODIFIED SPECIFICATION >>> (Thu, 11 Jul 2019 21:36:31 +0900)
						if ( curr instanceof Pair ) {
							Pair p = (Pair) curr;
							ArrayList al = new ArrayList();
							al.add( parent );
							al.addAll( Arrays.asList( p.toArray() ) );
							guiBuild( al.toArray() );
						} else if ( curr instanceof Procedure ) {
							((Procedure)curr).apply1( parent  );
						} else if ( curr instanceof EmptyList ) {
							// 
						} else {
							guiAdd( parent, (Component)curr );
						}
						// MODIFIED SPECIFICATION <<< (Thu, 11 Jul 2019 21:36:31 +0900)
						mode = null;
					}
				}
			} catch ( Throwable e ) {
				throw new RuntimeException( "an error is occured in line " + ( i ) ,e );
			}
		}
		return parent;
	}	

	public void guiName( Container parent, String name ) {
		if ( parent == null )
			parent = userPane;
		
		if ( parent instanceof JNamedPanel ) {
			((JNamedPanel)parent).setNextComponentName( name );
		} else {
			logWarn( "WARNING guiName: the parent is not JNamedPanel" );
		}
	}
	public void guiConstraint( Container parent, Object constraint ) {
		if ( parent == null )
			parent = userPane;
		
		if ( parent instanceof JNamedPanel ) {
			((JNamedPanel)parent).setNextConstraint( constraint );
		} else {
			logWarn( "WARNING guiConstraint: the parent is not JNamedPanel" );
		}
	}
	public void guiProperty( Container parent, List<Object> propertyValues ) {
		if ( parent == null )
			parent = userPane;
		
		if ( parent instanceof JNamedPanel ) {
			((JNamedPanel)parent).setNextProperty( propertyValues );
		} else {
			logWarn( "WARNING guiProperty: the parent is not JNamedPanel" );
		}
	}
	public void guiNextProcedure( Container parent, Procedure proc ) {
		if ( parent == null )
			parent = userPane;
		
		if ( parent instanceof JNamedPanel ) {
			((JNamedPanel)parent).setNextProcedure( proc );
		} else {
			logWarn( "WARNING guiProperty: the parent is not JNamedPanel" );
		}
	}
	public void guiNextIndex( Container parent, int nextIndex ) {
		if ( parent == null )
			parent = userPane;
		
		if ( parent instanceof JNamedPanel ) {
			((JNamedPanel)parent).setNextIndex( nextIndex );
		} else {
			logWarn( "WARNING guiProperty: the parent is not JNamedPanel" );
		}
	}
	public void guiNewline( Container parent ) {
		if ( parent == null )
			parent = userPane;
		parent.add( FlawLayout.createNewLine() );
	}
	public void guiValidate(Container parent) {
		parent.validate();
	}
	public void guiInvalidate(Container parent) {
		parent.invalidate();
	}
	public void guiRevalidate(Container parent) {
		parent.revalidate();
	}
	public void guiRepaint(Container parent) {
		parent.repaint();;
	}

	
	public void updateFilename(File file) {
		if ( tf_currentFile != null ) {
			tf_currentFile.setText( file !=null ? file.getPath() : ""  );
		}
		
		if ( cb_relatedFiles != null ) {
			int i = pulsar.relatedFiles.indexOf( file );
			if ( 0<=i ) {
				if ( ! isComboBoxUpdating  ) {
					try { 
						isComboBoxUpdating = true;
						this.cb_relatedFiles.setSelectedIndex( i );
					} finally {
						isComboBoxUpdating = false;
					}
				}
			}
		}
	}
	
	public void newlineGui() {
		// userPane.add( Box.createVerticalStrut(500 ) );
	}
	
	public final Action NEW_SCRATCHPAD = new AbstractAction() {
		@SuppressWarnings("unused")
		@Override
		public void actionPerformed(ActionEvent e) {
			PulsarScratchPad scratchPad = new PulsarScratchPad() {
				@Override
				public Scheme getScheme() {
					return pulsar.getScheme();
				}
			}.initialize();
		}
		{
			putValue( Action2.NAME, "New Scratchpad" );
			putValue( Action.MNEMONIC_KEY, (int)'n' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_F4, ActionEvent.SHIFT_MASK) );
		}
	};

	public final Action EDIT_SCRATCHPAD = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			PulsarScratchPad scratchPad = new PulsarScratchPad() {
				@Override
				public Scheme getScheme() {
					return pulsar.getScheme();
				}
			};
			try {
				scratchPad.openFile( pulsar.getMainFile() );
			} catch (IOException e1) {
				logError("", e1);
			}
		}
		{
			putValue( Action2.NAME, "Edit Main File" );
			putValue( Action.MNEMONIC_KEY, (int)'e' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_E, ActionEvent.CTRL_MASK ) );
		}
	};

	public final Action RESET_SEQUENCER = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			pulsar.reset();
		}
		{
			putValue( Action2.NAME, "Reset the Sequencer" );
			putValue( Action.MNEMONIC_KEY, (int)'r' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_R, ActionEvent.CTRL_MASK) );
		}
	};
	


	public final Action SET_MAIN_FILE_ACTION = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			JFileChooser fc = new JFileChooser();
			fc.setFileFilter( new FileFilter() {
				@Override
				public String getDescription() {
					return "*.scm (scheme)";
				}
				@Override
				public boolean accept(File f) {
					return ! f.isFile() || f.getName().endsWith( "scm" );
				}
			});
			{
				File mainFile = pulsar.getMainFile();
				if ( mainFile != null ) {
					fc.setSelectedFile( mainFile );
				}
			}
			int result = fc.showOpenDialog( frame );
			if ( result == JFileChooser.APPROVE_OPTION ) {
				pulsar.close();
				pulsar.setMainFile( null, fc.getSelectedFile() );
			}
		}
		{
			putValue( Action2.NAME, "Set Main File" );
			putValue( Action.MNEMONIC_KEY, (int)'s' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK) );
		}
	};
	public final Action CLEAR_MAIN_FILE_ACTION = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			pulsar.close();
			pulsar.setMainFile( null, null );
		}
		{
			putValue( Action2.NAME, "Clear Main File" );
			putValue( Action.MNEMONIC_KEY, (int)'c' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_W, ActionEvent.CTRL_MASK) );
		}
	};
	public final Action TOGGLE_PLAYING_ACTION = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			pulsar.togglePlaying();
		}
		{
			putValue( Action2.NAME, "Play/Stop" );
			putValue( Action.MNEMONIC_KEY, (int)'P' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, ActionEvent.CTRL_MASK) );
		}
	};
	public final Action RESET_PLAYING_ACTION = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			pulsar.rewind();
		}
		{
			putValue( Action2.NAME, "Reset" );
			putValue( Action.MNEMONIC_KEY, (int)'R' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_HOME, ActionEvent.CTRL_MASK) );
		}
	};
	public final Action TAP_TEMPO_ACTION = new AbstractAction() {
		@Override
		public void actionPerformed(ActionEvent e) {
			pulsar.tempoTapper.tap();
		}
		{
			putValue( Action2.NAME, "Tap Tempo" );
			putValue( Action.MNEMONIC_KEY, (int)'T' );
			putValue( Action.ACCELERATOR_KEY , KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, ActionEvent.CTRL_MASK) );
		}
	};


	protected class JPulsarFrame extends PulsarScratchPad {
		public JPulsarFrame( String title ) {
			super();
		}
		public JPulsarFrame() {
			this("Pulsar");
		}
		@Override
		public Scheme getScheme() {
			return PulsarGui.this.pulsar.getScheme();
		}

		{
			setTitle( "Pulsar - a Lisp Scheme Music Sequencer" );
//			JMenuBar menuBar = new JMenuBar();
//			setJMenuBar(menuBar);
			
			JMenuBar menuBar = getJMenuBar();
			
			{
				JMenu m = new JMenu( "Sequencer" );
				m.setMnemonic( 's' );

//				m.add( new JMenuItem( NEW_SCRATCHPAD ) );
				m.add( new JMenuItem( SET_MAIN_FILE_ACTION ) );
				m.add( new JMenuItem( CLEAR_MAIN_FILE_ACTION ) );
//				m.add( new JMenuItem( EDIT_SCRATCHPAD ) );
				m.add( new JMenuItem( RESET_SEQUENCER ) );
				menuBar.add( m,0 );
			}
			
			{
				JMenu m = new JMenu( "Play" );
				m.setMnemonic( 'P' );

				m.add( new JMenuItem( TOGGLE_PLAYING_ACTION ) );
				m.add( new JMenuItem( RESET_PLAYING_ACTION ) );
				m.add( new JMenuItem( TAP_TEMPO_ACTION ) );
				menuBar.add( m );
			}
			{
				JMenu m = new JMenu( "View" );
				m.setMnemonic( 'v' );

				{
					JMenu mm = new JMenu( "Tempo Range" );
					for ( PulsarGui.TempoRange r :  TempoRange.values() ) {
						mm.add( new JMenuItem( r.createAction( PulsarGui.this ) ) );
					}
					m.add( mm );
				}

				{
					JMenu mm = new JMenu( "Panel Orientation" );
					for ( PulsarGui.PanelOrientation r :  PanelOrientation.values() ) {
						mm.add( new JMenuItem( r.createSetPanelOrientationAction( PulsarGui.this ) ) );
					}
					m.add( mm );
				}
				
				{
					JMenu mm = new JMenu( "Scratchpad Orientation" );
					for ( PulsarGui.PanelOrientation r :  PanelOrientation.values() ) {
						mm.add( new JMenuItem( r.createSetScratchpadOrientationAction( PulsarGui.this ) ) );
					}
					m.add( mm );
				}

				menuBar.add( m );
			}
			
			Action2.processMenuBar(menuBar);
		}
		
		
		JComponent pulsarRootPane;
		JPanel staticPane;
		JNamedPanel userPane;
		{
			this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

			staticPane = new JPanel() {
				@Override
				public Dimension getPreferredSize() {
					return new Dimension(500,250);
				}
			};
			
			staticPane.setLayout( new BorderLayout(BORDER_SIZE,BORDER_SIZE) );

			JPanel staticPaneOuter = staticPane; 

			userPane   = new JNamedPanel() {
				@Override
				public Dimension getMinimumSize() {
					return new Dimension(0,0);
				}
			};
			userPane.setLayout(new FlawLayout());

			//Create and set up the content pane.
			JScrollPane userPaneOuter = new JScrollPane( userPane );
//			userPaneOuter.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );
//			userPaneOuter.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED );
			userPaneOuter.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER );
			userPaneOuter.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER );
			// SEE TAG_PACK_TWICE
			
			if ( false ) {
				pulsarRootPane  = new JSplitPane( JSplitPane.VERTICAL_SPLIT, 
						staticPane,
						userPaneOuter
						);
				((JSplitPane)pulsarRootPane).setContinuousLayout( true );
				((JSplitPane)pulsarRootPane).setDividerSize(5);
				((JSplitPane)pulsarRootPane).setDividerLocation(500);

				// See : 
				//   Detecting JSplitPane Divider Movement 
				//   https://stackoverflow.com/questions/14468648/detecting-jsplitpane-divider-movement
				pulsarRootPane.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, 
						new PropertyChangeListener() {
					@Override
					public void propertyChange(PropertyChangeEvent pce) {
						userPane.revalidate();
						staticPane.revalidate();
					}
				});
			} else {
				pulsarRootPane  = new JPanel( new BorderLayout() );
				pulsarRootPane.add( staticPaneOuter, BorderLayout.PAGE_START);
				pulsarRootPane.add( userPaneOuter, BorderLayout.CENTER );
				pulsarRootPane.setMaximumSize( new Dimension( 500, 400 ));
				
			}
			
			
//				staticPane.setMaximumSize( new Dimension(0, 0 ));
//				userPane.setMaximumSize( new Dimension(0, 0 ));
			if ( false ) {
				this.getContentPane().add ( pulsarRootPane );
			} else {
				this.scratchPadRoot.remove( this.scrollPane );
				this.scratchPadRoot.add( this.scrollPane, BorderLayout.CENTER );
				this.scratchPadRoot.add( this.pulsarRootPane, BorderLayout.PAGE_START );
				this.scratchPadRoot.revalidate();
			}

			// Tempo Button
	        staticPane.add( new JPusarFilePanel(), BorderLayout.PAGE_START );
			staticPane.add( createStartStopButton(), BorderLayout.LINE_END );
			staticPane.add( createTempoTapButton(), BorderLayout.CENTER );
			staticPane.add( createRewindButton(), BorderLayout.LINE_START );
//			REMOVED >>> (Mon, 08 Jul 2019 22:06:16 +0900)
//			staticPane.add( createCueButton(), BorderLayout.PAGE_END );
//			REMOVED <<< (Mon, 08 Jul 2019 22:06:16 +0900)

			// createEmptyBorder( top left bottom right )
//				staticPane.setBorder( BorderFactory.createEmptyBorder(10,20,5,20) );
//				userPane.setBorder(   BorderFactory.createEmptyBorder(5,20,20,20) );

//				((JComponent)pulsarRootPane).setBorder( BorderFactory.createEmptyBorder() );
			((JComponent)pulsarRootPane).setBorder( BorderFactory.createEmptyBorder(10,10,10,10) );

			// frame.setMaximizedBounds(new Rectangle(0, 0, 400, 1000));
			this.pack();
			this.setSize( 700, 500 );
			this.setVisible(true);
			this.addComponentListener( new ComponentAdapter() {
				@Override
				public void componentResized(ComponentEvent e) {
					JRootPane pane = JPulsarFrame. this.getRootPane();
					for (Component c : pane.getComponents() ) {
						c.revalidate();
					}
					pane.revalidate();
					pane.validate();
					
				}
			});
			
	        PulsarGui.this.rootPane = pulsarRootPane;
	        PulsarGui.this.staticPane = staticPane;
	        PulsarGui.this.userPane = userPane;
	        PulsarGui.this.staticPaneOuter = staticPaneOuter;
	        PulsarGui.this.userPaneOuter = userPaneOuter;
			
		}
		
		JProgressBar pb_position = new JProgressBar() {
			@Override
			public Dimension getPreferredSize() {
				Dimension s = super.getPreferredSize();
				s.height = 40;
				return s;
			}
		};
		{
			PulsarGui.this.pb_position = pb_position; 
			add( pb_position , BorderLayout.PAGE_END );
			pb_position.setBorder( BorderFactory.createCompoundBorder(
					BorderFactory.createEmptyBorder(10,10,10,10),
					pb_position.getBorder()) );
//				pb_position.setBorder( BorderFactory.createEmptyBorder(0,0,0,0) );
			pb_position.setMaximum( Pulsar.PB_POSITION_MAX );
			pb_position.setMinimum(0);
		}

		
		public void guiSetScratchpadOrientation( PulsarGui.PanelOrientation panelOrientation ) {
			switch ( panelOrientation ) {
				case TOP :
					this.scratchPadRoot.remove( this.scrollPane );
					this.scratchPadRoot.remove( this.pulsarRootPane );
					this.scratchPadRoot.add( this.scrollPane, BorderLayout.CENTER );
					this.scratchPadRoot.add( this.pulsarRootPane, BorderLayout.PAGE_END );
					this.scratchPadRoot.revalidate();
					break;
				case RIGHT :
					this.scratchPadRoot.remove( this.scrollPane );
					this.scratchPadRoot.remove( this.pulsarRootPane );
					this.scratchPadRoot.add( this.scrollPane, BorderLayout.CENTER );
					this.scratchPadRoot.add( this.pulsarRootPane, BorderLayout.LINE_START );
					this.scratchPadRoot.revalidate();
					break;
				case BOTTOM :
					this.scratchPadRoot.remove( this.scrollPane );
					this.scratchPadRoot.remove( this.pulsarRootPane );
					this.scratchPadRoot.add( this.scrollPane, BorderLayout.CENTER );
					this.scratchPadRoot.add( this.pulsarRootPane, BorderLayout.PAGE_START );
					this.scratchPadRoot.revalidate();
					break;
				case LEFT :
					this.scratchPadRoot.remove( this.scrollPane );
					this.scratchPadRoot.remove( this.pulsarRootPane );
					this.scratchPadRoot.add( this.scrollPane, BorderLayout.CENTER );
					this.scratchPadRoot.add( this.pulsarRootPane, BorderLayout.LINE_END );
					this.scratchPadRoot.revalidate();
					break;
				default :
					throw new RuntimeException( "internal error" );
			}
		}

	}

    /*
     * === Meaning of these variable name for panels ===
     * ex) panel_p0_1_2
     * These numbers denote its position.
     *    ___________________
     *    |                 |
     *    |        1        |
     *    |_________________|
     *    |   |         |   |
     *    | 4 |    0    | 2 |
     *    |___|_________|___|
     *    |                 |
     *    |        3        |
     *    |_________________|
     * 
     * The variable name "panel_p0_1_2" denotes that 
     * it is the panel on the position No.2 inside the
     * panel on the position No.1 inside the panel on 
     * the position No.0 .
     * 
     */


    class JPusarFilePanel extends JPanel {
    	public JPusarFilePanel() {
    		super( newLayout() );
    	}
    	JPanel panel_outer = new JPanelExtentionOuter();
    	{
    		add( panel_outer, BorderLayout.CENTER );
    	}

    	class JPanelExtentionOuter extends JPanel {
    		public JPanelExtentionOuter() {
    			super( newLayout() );
    		}

        	JPanel panel_exec = new JPanelExtentionExec();
        	{
//        		add( panel_exec, BorderLayout.CENTER );
        	}
    		class JPanelExtentionExec extends JPanel {
    			public JPanelExtentionExec() {
    				super( newLayout() );
    			}
    			JButton execButton = new JButton( "EXEC" ) {
    				@Override
    				public Dimension getPreferredSize() {
    					Dimension s = super.getPreferredSize();
    					s.width = 75;
    					return s;
    				}
    			};
    			{
    				execButton.addActionListener( new ActionListener() {
    					@Override
    					public void actionPerformed(ActionEvent e) {
    						int i = cb_relatedFiles.getSelectedIndex();
    						if ( 0<=i ) {
    							File file = pulsar.relatedFiles.get(i);
    							pulsar.setMainFile( pulsar.relatedFileParent, file );
    						}
    					}
    				});
    				add( execButton , BorderLayout.LINE_START );
    			}

    			JComboBox<String> cb_relatedFiles = new JComboBox<String>() {
    			};
    			{
    				PulsarGui.this.cb_relatedFiles = cb_relatedFiles;
    				cb_relatedFiles.setEditable(false);
    				cb_relatedFiles.addPopupMenuListener( new PopupMenuListener() {
    					@Override
    					public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
    						logInfo("popupMenuWillBecomeVisible()");
    						// readHistoryFile(comboBox);
    					}
    					@Override
    					public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
    						logInfo( "popupMenuWillBecomeInvisible()");
    					}
    					@Override
    					public void popupMenuCanceled(PopupMenuEvent e) {
    						logInfo(".popupMenuCanceled()");
    					}
    				});
    				cb_relatedFiles.addItemListener( new ItemListener() {
    					@Override
    					public void itemStateChanged(ItemEvent e) {
    						logInfo("Pulsar.createFilePanel().new ItemListener() {...}.itemStateChanged()");
    						if ( e.getStateChange() == ItemEvent.SELECTED ) {
    							// Do nothing.
    						}
    					}
    				});
    				add( cb_relatedFiles, BorderLayout.CENTER );
    			}
    		}

    		JPanel panel_openFile = new JOpenFilePanel();
        	{
        		add( panel_openFile, BorderLayout.CENTER );
        	}
        	class JOpenFilePanel extends JPanel {
				JOpenFilePanel(){ super( newLayout() ); }

				JPanel panel_fileButtons = new JFileButtonsPanel();
				{
//					REMOVED >>> (Wed, 10 Jul 2019 01:34:10 +0900)
//					add( panel_fileButtons, BorderLayout.LINE_START );
//					REMOVED <<< (Wed, 10 Jul 2019 01:34:10 +0900)
				}
				class JFileButtonsPanel extends JPanel {
					JFileButtonsPanel(){ super( new FlawLayout() ); }
					JButton openMainFileButton = new JButton( "SET" );
					{
//						REMOVED >>> (Wed, 10 Jul 2019 01:34:10 +0900)
//						add( openMainFileButton  );
//						REMOVED <<< (Wed, 10 Jul 2019 01:34:10 +0900)
						openMainFileButton.addActionListener( new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								SET_MAIN_FILE_ACTION.actionPerformed(e);
							}
						} );
					}
					JButton clearMainFileButton = new JButton( "CLR" );
					{
//						REMOVED >>> (Wed, 10 Jul 2019 01:34:10 +0900)
//						add( clearMainFileButton );
//						REMOVED <<< (Wed, 10 Jul 2019 01:34:10 +0900)
						clearMainFileButton.addActionListener( new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								CLEAR_MAIN_FILE_ACTION.actionPerformed(e);
							}
						} );
					}
				}
        		JTextField currentFile = new JTextField();
        		{
        			tf_currentFile = currentFile;
        			this.add( currentFile, BorderLayout.CENTER );
        			currentFile.setEditable(false);
        			currentFile.setBorder( new TitledBorder( currentFile.getBorder(), "MAIN FILE" ) );
        		}
        	}
    	}
    	JButton resetButton = new JButton( "⟳" ) {
    		@Override
    		public Dimension getPreferredSize() {
    			return new Dimension( 75,super.getPreferredSize().height + 20 );
    		}
    	};
    	{
    		add( resetButton , BorderLayout.LINE_END );
    		resetButton.addActionListener( new ActionListener() {
    			@Override
    			public void actionPerformed(ActionEvent e) {
    				pulsar.reset();
    			}
    		});
    	}

    	

//	    	ImageIcon resetIcon;
//	    	{
//	    		resetIcon = loadIcon( "reset.png" );
//	    	}
//			public ImageIcon loadIcon( String path )  {
//				try {
//					return new ImageIcon( ImageIO.read(getClass().getResource( path )) );
//				} catch (IOException e) {
//					logError("", e);
//					return null;
//				}
//			}
		
    	JSliderPanel panel_slider = new JSliderPanel();
    	{
				add( panel_slider, BorderLayout.PAGE_END );
    	}
    	class JSliderPanel extends JPanel {
    		public JSliderPanel() {
    			super( newLayout() );
			}
    		// *** UNUSED *** (Tue, 09 Jul 2019 18:04:26 +0900)
    		JTempoScalePanel panel_tempoScale = new JTempoScalePanel();
    		{
//    			add( panel_tempoScale, BorderLayout.CENTER );
    		}
    		class JTempoScalePanel extends JPanel {
    			private final class TempoRangeActionListener implements ActionListener {
					private final PulsarGui.TempoRange tempoRange;
					private TempoRangeActionListener(PulsarGui.TempoRange r) {
						this.tempoRange = r;
					}
					@Override
					public void actionPerformed(ActionEvent e) {
						guiSetTempoRange( tempoRange );
					}
				}
				
    			public JTempoScalePanel() {
    				super( new FlawLayout( FlawLayout.LEFT ) );
    				setBorder( BorderFactory.createTitledBorder("Tempo Range"));
				}
    			ButtonGroup group = new ButtonGroup();
    			{
    				 for ( PulsarGui.TempoRange r :  TempoRange.values() ) {
//    					 JPulsarRadioButton b = new JPulsarRadioButton( r.caption );
//    					 b.addActionListener( new TempoRangeActionListener(r));
    					 JPulsarRadioButton b = new JPulsarRadioButton( r.createAction( PulsarGui.this ) );
    					 Action2.processButton(b);
    					 group.add( b );
    					 add( b );
    				 }
    			}
    		}
    		
    		// *** UNUSED *** (Tue, 09 Jul 2019 18:04:26 +0900)
    		JPanelOrientationPanel panelOrientationPanel = new JPanelOrientationPanel();
    		{
//    			add(  panelOrientationPanel, BorderLayout.LINE_END );
    		}
    		class JPanelOrientationPanel extends JPanel {
				
    			public JPanelOrientationPanel() {
    				super( new FlawLayout( FlawLayout.LEFT ) );
    				setBorder( BorderFactory.createTitledBorder( "Orientation" ) );
				}
    			ButtonGroup group = new ButtonGroup();
    			{
    				 for ( PulsarGui.PanelOrientation r :  PanelOrientation.values() ) {
//    					 JPulsarRadioButton b = new JPulsarRadioButton( r.caption );
//    					 b.addActionListener( new JPanelOrientationActionListener(r));
    					 JPulsarRadioButton b = new JPulsarRadioButton( r.createSetPanelOrientationAction( PulsarGui.this ) );
    					 Action2.processButton(b);
//    					 b.addActionListener( new JPanelOrientationActionListener(r));
    					 group.add( b );
    					 add( b );
    					 orientaionMap.put(r, b );
    					 if ( DEFAULT_PANEL_ORIENTATION.equals( r )) {
    						 b.setSelected(true);
    					 }
    				 }
    			}
    		}

    		JSlider sl_tempoSlider = new JSlider();
    		{
    			PulsarGui.this.sl_tempoSlider = sl_tempoSlider;
//    			add( sl_tempoSlider, BorderLayout.PAGE_END );
    			add( sl_tempoSlider, BorderLayout.CENTER );
    			sl_tempoSlider.setBorder( BorderFactory.createEmptyBorder() );
    			sl_tempoSlider.setMinimum(1);
    			sl_tempoSlider.setMaximum(1000);
    			sl_tempoSlider.setPaintTicks(true);
    			sl_tempoSlider.setPaintTrack( true);
    			sl_tempoSlider.setMajorTickSpacing(100);
    			sl_tempoSlider.setMinorTickSpacing(25);
    			sl_tempoSlider.setPreferredSize( new Dimension(500, 80));
    			Dictionary<Integer,JLabel> labelTables = new Hashtable<>();
    			labelTables.put(10, new JLabel( "10" ));
    			labelTables.put(50, new JLabel( "50" ));
    			labelTables.put(100, new JLabel( "100" ));
    			labelTables.put(150, new JLabel( "150" ));
    			labelTables.put(200, new JLabel( "200" ));
    			labelTables.put(300, new JLabel( "300" ));
    			labelTables.put(400, new JLabel( "400" ));
    			labelTables.put(500, new JLabel( "500" ));
    			labelTables.put(750, new JLabel( "750" ));
    			labelTables.put(1000, new JLabel( "1000" ));

    			sl_tempoSlider.setLabelTable( labelTables );
    			sl_tempoSlider.setPaintLabels(true);
    			sl_tempoSlider.addChangeListener(new ChangeListener() {
    				@Override
    				public void stateChanged(ChangeEvent e) {
    					try {
    						//    					logInfo( "TempoSlider : " + ((JSlider)e.getSource()).getValue() );
    						pulsar.tempoTapper.setBeatsPerMinute( ((JSlider)e.getSource()).getValue() );
    					} catch (JackException e1) {
    						logError("", e1);
    					}
    				}
    			});
    			
    			sl_tempoSlider.setBorder(
    					BorderFactory.createCompoundBorder(
    							BorderFactory.createTitledBorder("TEMPO"),
    							sl_tempoSlider.getBorder()
    							)
    					);
    			
    			// InitializeTempoSlider
    			pulsar.tempoTapper.registerNotifier( new TempoTapperTempoNotifier() {
    				@Override
    				public void notifyTempo(double beatPerMinute) {
    					sl_tempoSlider.setValue( (int)beatPerMinute );
    				}
    			});
    			
    			//
    			guiSetTempoRange( TempoRange.NORMAL );
    		}
    	}
    }

	void initializeTempoSlider() {
	}

    private JButton createStartStopButton() {
    	JButton b = new JButton( "■|▶" ) {
    		@Override
    		public Dimension getPreferredSize() {
    			Dimension s = super.getPreferredSize();
    			s.width =  75;
    			return s;
    		}
    	};
    	b.addActionListener( new ActionListener() {
    		@Override
    		public void actionPerformed(ActionEvent e) {
    			pulsar.togglePlaying();
    		}
    	});
    	return b;
    }
    
    private JButton createRewindButton() {
    	JButton b = new JButton( "||◀" ) {
    		@Override
    		public Dimension getPreferredSize() {
    			Dimension s = super.getPreferredSize();
    			s.width =  75;
    			return s;
    		}
    	};
    	b.addActionListener( new ActionListener() {
    		@Override
    		public void actionPerformed(ActionEvent e) {
    			pulsar.rewind();
    		}
    	});
    	return b;
    }

    private JButton createCueButton() {
    	JButton b = new JButton( "=== CUE ===" ) {
    		@Override
    		public Dimension getPreferredSize() {
    			return new Dimension( super.getPreferredSize().width, 100 );
    		}
    	};
    	b.addActionListener( new ActionListener() {
    		@Override
    		public void actionPerformed(ActionEvent e) {
    			pulsar.cue();
    		}
		});
		return b;
	}
    
	private JButton createTempoTapButton() {
    	JButton tempoTapButton = new JButton( "TEMPO" );
    	tempoTapButton.addActionListener( new ActionListener() {
    		@Override
    		public void actionPerformed(ActionEvent e) {
    			pulsar.tempoTapper.tap();
    		}
    	});

    	pulsar.tempoTapper.registerNotifier( new TempoTapperTempoNotifier() {
			@Override
			public void notifyTempo(double beatPerMinute) {
				tempoTapButton.setText( String.format( "Tempo=%.2f", beatPerMinute  ) );
			}
		} );

    	tempoTapButton.setPreferredSize(new Dimension(200, 100));
    	tempoTapButton.setMargin(new Insets(20, 20, 20, 20));

    	return tempoTapButton;
	}
}