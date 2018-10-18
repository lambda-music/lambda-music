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

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.filechooser.FileFilter;

import org.jaudiolibs.jnajack.JackException;

import ats.kawapad.PulsarScratchPad;
import ats.pulsar.Pulsar.TempoTapperTempoNotifier;
import ats.pulsar.lib.FlawLayout;
import ats.pulsar.lib.JNamedPanel;
import ats.pulsar.lib.JPulsarRadioButton;
import ats.pulsar.lib.JSelectableUserObject;
import ats.pulsar.lib.LayoutUtils;
import ats.pulsar.lib.SpringLayoutUtil;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.ProcedureN;
import gnu.mapping.Symbol;
import kawa.standard.Scheme;

class PulsarGui {
	static void logError( String msg, Throwable e ) {
        Logger.getLogger(Pulsar.class.getName()).log(Level.SEVERE, msg, e);
//		System.err.println( msg );
	}
	static void logInfo( String msg ) {
//        Logger.getLogger(Pulsar.class.getName()).log(Level.INFO, msg);
		System.err.println( msg );
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
				try {
					return SchemeNewFactory.process( pulsar, args);
				} catch ( Exception e ) {
					logError("", e);
					return null;
				}
    		}
    	});

    	SchemeUtils.defineVar( scheme, "gui-parent" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
    			if ( 1 <= args.length ) {
        			ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
        			Component component = (Component) SchemeUtils.schemeNullToJavaNull( argList.pop() );
    				Container parent = component.getParent();
					return parent;
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
    	SchemeUtils.defineVar( scheme, "gui-repaint" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
//					logInfo("refresh-gui");
				guiRepaint();
    			return EmptyList.emptyList;
    		}
    	});
    	SchemeUtils.defineVar( scheme, "gui-layout!" , new ProcedureN() {
			@Override
    		public Object applyN(Object[] args) throws Throwable {
				logInfo( "gui-layout" );
				
				ArrayList<Object> argList = new ArrayList<>( Arrays.asList(args) );
				
				if ( args.length == 0 ) {
					throw new RuntimeException( "gui-layout! (panel) ['gridbag | 'spring | 'flow | 'grid]" );
				} else if ( args.length == 1 ) {
					guiLayout( userPane, SchemeUtils.symbolToString( args[0] ) );
				} else if ( 2 <= args.length ) {
					Container container = (Container) argList.remove(0);
					String type         = SchemeUtils.symbolToString( argList.remove(0) );
					int row  = 0 < argList.size() ? SchemeUtils.toInteger( argList.remove(0)) : 1;
					int col  = 0 < argList.size() ? SchemeUtils.toInteger( argList.remove(0)) : 0;
					int hgap = 0 < argList.size() ? SchemeUtils.toInteger( argList.remove(0)) : 0;
					int vgap = 0 < argList.size() ? SchemeUtils.toInteger( argList.remove(0)) : 0;
					guiLayout( container, type, row, col, hgap, vgap );
				} 
    			return EmptyList.emptyList;
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
				guiRepaint();
    			return EmptyList.emptyList;
    		}
    	});
    	
    	PulsarScratchPad.initScheme( scheme );

    }
	
	//Create the "cards".
    JFrame frame;
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
		// frame.pack();
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
		
		frame.pack();
		frame.pack();
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
	public void guiGridBagLayout(Container userPane) {
		userPane.setLayout( new GridBagLayout() );
	}
	public void guiGridLayout( Container userPane, int row, int col, int hgap, int vgap  ) {
		logInfo( "gridLayout : " + row + " / " + col + " / " + hgap + " / " + vgap );
		userPane.setLayout( new GridLayout( row, col, hgap, vgap ) );
	}
	public void guiLayout( Container container, String type, Object ... args  ) {
		switch ( type ) {
			case "default":
			case "flow" :
				guiFlowLayout( container );
				break;
			case "border" :
				guiBorderLayout( container );
				break;
			case "spring" :
				guiSpringLayout( container );
				break;
			case "grid" :
				guiGridLayout( container, (int)args[0], (int)args[1], (int)args[2], (int)args[3] );
				break;
			case "gridbag" :
				guiGridBagLayout( container );
				break;
			default :
				throw new RuntimeException( "Unknown LayoutManager name : " + type );
		}
	}
	public void guiRepaint() {
		userPane.revalidate();
		userPane.repaint();
        // frame.pack();
	}
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
		
//			JSplitPane rootPane = (JSplitPane) this.rootPane;
		
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
							guiAdd( parent, (Component)car, cdr );
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
	

	public void guiName( Container parent, String name ) {
		if ( parent == null )
			parent = userPane;
		
		if ( parent instanceof JNamedPanel ) {
			((JNamedPanel)parent).setNextComponentName( name );
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

	
	public void updateFilename(File file) {
		if ( tf_currentFile != null ) {
			tf_currentFile.setText( file.getPath() );
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

	class JPulsarFrame extends JFrame {
		public JPulsarFrame() {
			super( "Pulsar" );
		}
		
		{
			JMenuBar menuBar = new JMenuBar();
			setJMenuBar(menuBar);
			
			JMenu m = new JMenu( "File" );
			m.setMnemonic( 'f' );
			JMenuItem menuItem1 = new JMenuItem( "Scratch Pad" );
			menuItem1.setAccelerator( KeyStroke.getKeyStroke(KeyEvent.VK_F4, ActionEvent.SHIFT_MASK ) );
			
			menuItem1.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					new PulsarScratchPad() {
						@Override
						public Scheme getScheme() {
							return pulsar.scheme;
						}
					};
				}
			});
			m.add( menuItem1 );
			menuBar.add( m );
		}
		
		
		JComponent rootPane;
		JPanel staticPane;
		JNamedPanel userPane;
		{
			this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

			staticPane = new JPanel() {
				@Override
				public Dimension getPreferredSize() {
					return new Dimension(500,400);
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
			userPaneOuter.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );
			userPaneOuter.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED );
			// SEE TAG_PACK_TWICE
			
			if ( false ) {
				rootPane  = new JSplitPane( JSplitPane.VERTICAL_SPLIT, 
						staticPane,
						userPaneOuter
						);
				((JSplitPane)rootPane).setContinuousLayout( true );
				((JSplitPane)rootPane).setDividerSize(5);
				((JSplitPane)rootPane).setDividerLocation(500);

				// See : 
				//   Detecting JSplitPane Divider Movement 
				//   https://stackoverflow.com/questions/14468648/detecting-jsplitpane-divider-movement
				rootPane.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, 
						new PropertyChangeListener() {
					@Override
					public void propertyChange(PropertyChangeEvent pce) {
						userPane.revalidate();
						staticPane.revalidate();
					}
				});
			} else {
				rootPane  = new JPanel( new BorderLayout() );
				rootPane.add( staticPaneOuter, BorderLayout.PAGE_START);
				rootPane.add( userPaneOuter, BorderLayout.CENTER );
				rootPane.setMaximumSize( new Dimension( 500, 400 ));
				
			}
			
			
//				staticPane.setMaximumSize( new Dimension(0, 0 ));
//				userPane.setMaximumSize( new Dimension(0, 0 ));
			
			this.getContentPane().add ( rootPane );

			// Tempo Button
	        staticPane.add( new JPusarFilePanel(), BorderLayout.PAGE_START );
			staticPane.add( createStartStopButton(), BorderLayout.LINE_END );
			staticPane.add( createTempoTapButton(), BorderLayout.CENTER );
			staticPane.add( createRewindButton(), BorderLayout.LINE_START );
			staticPane.add( createCueButton(), BorderLayout.PAGE_END );

			// createEmptyBorder( top left bottom right )
//				staticPane.setBorder( BorderFactory.createEmptyBorder(10,20,5,20) );
//				userPane.setBorder(   BorderFactory.createEmptyBorder(5,20,20,20) );

//				((JComponent)rootPane).setBorder( BorderFactory.createEmptyBorder() );
			((JComponent)rootPane).setBorder( BorderFactory.createEmptyBorder(10,10,10,10) );

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
			
	        PulsarGui.this.rootPane = rootPane;
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
        		add( panel_exec, BorderLayout.CENTER );
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
        		add( panel_openFile, BorderLayout.PAGE_START );
        	}
        	class JOpenFilePanel extends JPanel {
        		JOpenFilePanel(){ super( newLayout() ); }

        		JButton openMainFileButton = new JButton( "OPEN" );
        		{	
        			this.add( openMainFileButton, BorderLayout.LINE_START );
        			openMainFileButton.addActionListener(new ActionListener() {
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
        					int result = fc.showOpenDialog( frame );
        					if ( result == JFileChooser.APPROVE_OPTION ) {
        						pulsar.close();
        						pulsar.setMainFile( null, fc.getSelectedFile() );
        					}
        				}
        			});
        		}
        		JTextField currentFile = new JTextField();
        		{
        			tf_currentFile = currentFile;
        			this.add( currentFile, BorderLayout.CENTER );
        			currentFile.setEditable(false);
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
//				add( panel_slider, BorderLayout.PAGE_END );
    	}
    	class JSliderPanel extends JPanel {
    		public JSliderPanel() {
    			super( newLayout() );
			}
    		JTempoScalePanel panel_tempoScale = new JTempoScalePanel();
    		{
    			add( panel_tempoScale, BorderLayout.CENTER );
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
    					 JPulsarRadioButton b = new JPulsarRadioButton( r.caption );
    					 b.addActionListener( new TempoRangeActionListener(r));
    					 group.add( b );
    					 add( b );
    				 }
    			}
    		}
    		
    		JPanelOrientationPanel panelOrientationPanel = new JPanelOrientationPanel();
    		{
    			add(  panelOrientationPanel, BorderLayout.LINE_END );
    		}
    		class JPanelOrientationPanel extends JPanel {
				private final class JPanelOrientationActionListener implements ActionListener {
					private final PulsarGui.PanelOrientation panelOrientation;
					private JPanelOrientationActionListener(PulsarGui.PanelOrientation panelOrientation) {
						this.panelOrientation = panelOrientation;
					}
					@Override
					public void actionPerformed(ActionEvent e) {
						guiSetPanelOrientation( panelOrientation );
					}
				}
				
    			public JPanelOrientationPanel() {
    				super( new FlawLayout( FlawLayout.LEFT ) );
    				setBorder( BorderFactory.createTitledBorder( "Orientation" ) );
				}
    			ButtonGroup group = new ButtonGroup();
    			{
    				 for ( PulsarGui.PanelOrientation r :  PanelOrientation.values() ) {
    					 JPulsarRadioButton b = new JPulsarRadioButton( r.caption );
    					 b.addActionListener( new JPanelOrientationActionListener(r));
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
    			add( sl_tempoSlider, BorderLayout.PAGE_END );
    			sl_tempoSlider.setBorder( BorderFactory.createEmptyBorder() );
    			sl_tempoSlider.setMinimum(1);
    			sl_tempoSlider.setMaximum(1000);
    			sl_tempoSlider.setPaintTicks(true);
    			sl_tempoSlider.setPaintTrack( true);
    			sl_tempoSlider.setMajorTickSpacing(100);
    			sl_tempoSlider.setMinorTickSpacing(25);
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