package ats.pulsar;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimerTask;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JSlider;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import ats.pulsar.lib.FlawLayout;
import ats.pulsar.lib.JNamedPanel;
import ats.pulsar.lib.JPulsarButton;
import ats.pulsar.lib.JPulsarCheckBox;
import ats.pulsar.lib.JPulsarComboBox;
import ats.pulsar.lib.JPulsarRadioButton;
import ats.pulsar.lib.JUserObjectContainer;
import ats.pulsar.lib.PulsarListItem;
import gnu.expr.Language;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.mapping.Symbol;
import gnu.math.IntNum;

public abstract class SchemeNewFactory {
	private static final Logger LOGGER = Logger.getLogger(Pulsar.class.getName());
	private static final Map<String, SchemeNewFactory> map = new HashMap<>();
	abstract Object create( Pulsar pulsar, List<Object> args );
	static void register( String key, SchemeNewFactory factory ) {
		if ( map.containsKey(key))
			throw new RuntimeException( "the key was already registered (" + key + ")" );

		map.put(key, factory );
	}

	static void logError( String msg, Throwable e ) {
        LOGGER.log(Level.SEVERE, msg, e);
	}
	static void logInfo( String msg ) {
//      LOGGER.log(Level.INFO, msg);
		System.err.println( msg );
	}
	static void logWarn( String msg ) {
		LOGGER.log(Level.WARNING, msg);
		System.err.println( msg );
	}

	public static Object process( Pulsar pulsar,  Object ... args ) {
		ArrayList<Object> arguments = new ArrayList<>( Arrays.asList( args ) );
		Object key = arguments.remove(0);
		if ( key == null )
			throw new RuntimeException("key is not specified");
		
		String keyString = SchemeUtils.anyToString(key);
		SchemeNewFactory factory = map.get(keyString);
		if ( factory == null ) {
			throw new RuntimeException("unknown object type (" + keyString + ")" );
		}
		Object result = factory.create( pulsar, arguments );

		return result;
	}



	static final class SchemeFunctionExecutor {
		private final Environment env;
		private final Language lang;
		private final Procedure procedure;
		private SchemeFunctionExecutor(Procedure procedure ) {
			this.env = Environment.getCurrent();
			this.lang = Language.getDefaultLanguage();
			this.procedure = procedure;
		}
		public Object execute( Object ... args ) {
			try {
				Environment.setCurrent(env);
				Language.setCurrentLanguage(lang);
				return this.procedure.applyN( args );
			} catch (Throwable e1) {
				logError( "" , e1 );
				return e1;
			}
		}
	}
	
	static final class ComponentFactoryActionListener implements ActionListener {
		SchemeFunctionExecutor executor; 
		ComponentFactoryActionListener(Procedure procedure) {
			this.executor = new SchemeFunctionExecutor(procedure); 
		}
		@Override
		public void actionPerformed(ActionEvent e) {
			AbstractButton button = (AbstractButton)e.getSource();
			executor.execute( 
					button.isSelected(),
					IString.valueOf( e.getActionCommand() ),
					((JUserObjectContainer)button).getUserObject(),
					e.getSource(), e 
					);
		}
	}

	static abstract class ComponentFactory {
		abstract Component create( ActionListener l, String caption, Object userObject );
		abstract ActionListener createActionListener( Procedure procedure );
	}
	static class JRadioButtonFactory extends ComponentFactory {
		ButtonGroup group = new ButtonGroup();
		@Override
		ActionListener createActionListener( Procedure procedure ) {
			return new ComponentFactoryActionListener( procedure );
		}
		@Override
		Component create(ActionListener l, String caption, Object userObject) {
			JPulsarRadioButton r = new JPulsarRadioButton( caption );
			r.setActionCommand( caption );
			r.addActionListener( l );
			r.setUserObject(userObject);
			group.add( r );
			return r;
		}
	}
	static class JCheckBoxFactory extends ComponentFactory {
		@Override
		ActionListener createActionListener( Procedure procedure ) {
			return new ComponentFactoryActionListener( procedure );
		}
		@Override
		Component create(ActionListener l, String caption, Object userObject) {
			JPulsarCheckBox r = new JPulsarCheckBox( caption );
			r.setActionCommand( caption );
			r.addActionListener( l );
			r.setUserObject(userObject);
			return r;
		}
	}
	
	static Object createSelectiveComponents( String type, List<Object> args, ComponentFactory f ) {
		if ( 1 < args.size()  ) {
			Procedure procedure;
			{
				int last = args.size() - 1;
				procedure = (Procedure) args.get( last );
				args.remove(last);
			}
			ActionListener listener = f.createActionListener( procedure );
			
			List<Object> result = new ArrayList();
			for ( Object e : args ) {
				if ( e instanceof Symbol ) {
					String symbol = SchemeUtils.symbolToString(e);
					switch ( symbol ) {
						case "selected" :
							if ( result.size() == 0 ) {
								throw new RuntimeException( "error : no item was created" );
							} else {
								((AbstractButton) result.get( result.size() - 1) ).setSelected(true);
							}
							break;
						default :
							
							break;
					}
				} else if ( e instanceof IString ) {
					String caption = SchemeUtils.toString(e);
					result.add( f.create(listener, caption, caption) );
				} else if ( e instanceof Pair ) {
					Pair p = (Pair) e;
					String caption = SchemeUtils.toString( p.getCar() );
					Object userObject = p.getCdr();
					result.add( f.create(listener, caption, userObject  ) );
				} else {
					
				}
			}
			return Pair.makeList( result );
		} else {
			throw new RuntimeException( "new " + type + "[ caption, caption, ..., procedure] " + args.size() );
		}
	}
	
	static {
		register( "label", new SchemeNewFactory() {
			@Override
			Object create(Pulsar pulsar, List<Object> args ) {
				if ( 0<args.size()  ) {
					return new JLabel( SchemeUtils.toString( args.get(0) ) );
				} else {
					return EmptyList.emptyList;
				}
			}
		});
		register( "panel", new SchemeNewFactory() {
			@Override
			Object create(Pulsar pulsar, List<Object> args ) {
				JNamedPanel panel = new JNamedPanel();

				if ( args.size() == 0 ) {
					pulsar.guiLayout(panel, "default" );
				} else if ( 1 <= args.size() ) {
					pulsar.guiLayout(panel, SchemeUtils.symbolToString( args.get(0) ) );
				}
				return panel;
			}
		});
		register( "group", new SchemeNewFactory() {
			@Override
			Object create(Pulsar pulsar, List<Object> args ) {
				JNamedPanel panel = new JNamedPanel();

				if ( args.size() == 0 ) {
					pulsar.guiLayout(panel, "default" );
				} else if ( 1 <= args.size() ) {
					String title = SchemeUtils.toString( args.get(0) );
					panel.setBorder( BorderFactory.createTitledBorder( title ) );

					if ( 2 <= args.size() ) {
						pulsar.guiLayout(panel, SchemeUtils.symbolToString( args.get(1) ) );
					} else {
						pulsar.guiLayout(panel, "flow" );
					}
				}
				return panel;
			}
		});

		register( "button", new SchemeNewFactory() {
			@Override
			Object create( Pulsar pulsar, List<Object> args ) {
				if ( args.size() == 2 ) {
					String caption;
					Object userObject;
					{
						Object arg0 = args.get(0);
						if ( arg0 instanceof Pair ) {
							Pair pair = (Pair) arg0;
							caption = SchemeUtils.toString( pair.getCar() );
							userObject = pair.getCdr();
						} else {
							caption = SchemeUtils.toString( arg0 );
							userObject = EmptyList.emptyList;
						}
					}

					Procedure procedure = (Procedure) args.get(1);

					Environment env = Environment.getCurrent();
					Language lang = Language.getDefaultLanguage();
					{
						JPulsarButton button = new JPulsarButton( caption );
						button.addActionListener( new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								try {
									Environment.setCurrent(env);
									Language.setCurrentLanguage(lang);

									AbstractButton button = (AbstractButton)e.getSource();
									
									procedure.applyN( new Object[] {
											true,
											SchemeUtils.toSchemeString( button.getText() ),
											((JUserObjectContainer)button).getUserObject(),
											button,
											e
											} );
								} catch (Throwable e1) {
									logError( "" , e1 );
								}
							}
						});
						button.setUserObject( userObject );
						return button;
					}
				} else {
					throw new RuntimeException( "new 'button has two parameters( caption proc )." + args.size() );
				}
			}
		});


		register( "timer", new SchemeNewFactory() {
			@Override
			Object create( Pulsar pulsar, List<Object> args ) {
				if ( 2 == args.size() ) {
					long interval = SchemeUtils.toLong(args.get(0));
					Procedure procedure = (Procedure)args.get(1);

					return createTimer(pulsar, interval, interval, procedure);
				} else if ( 3 <= args.size() ) {
					long delay = SchemeUtils.toLong(args.get(0));
					long interval = SchemeUtils.toLong(args.get(1));
					Procedure procedure = (Procedure)args.get(2);

					return createTimer(pulsar, delay, interval, procedure);

				} else {
					
					logInfo("WARNING : new 'timer was called but no argument was passed.");
					return EmptyList.emptyList;
				}
			}

			public Object createTimer(Pulsar pulsar, long delay, long interval, Procedure procedure) {
				SchemeFunctionExecutor executor = new SchemeFunctionExecutor( procedure );
				java.util.Timer timer = new java.util.Timer( true );
				timer.scheduleAtFixedRate( new TimerTask() {
					@Override
					public void run() {
						Object result = executor.execute();
						if ( Boolean.FALSE.equals( result ) ) {
							timer.cancel();
						}
					}
				}, delay, interval );

				pulsar.addCleanupHook( new Runnable() {
					@Override
					public void run() {
						timer.cancel();
					}
				});
				
				return new ProcedureN() {
					public Object applyN(Object[] args) throws Throwable {
						timer.cancel();
						return EmptyList.emptyList;
					};
				};
			}
		});


		register( "stimer", new SchemeNewFactory() {
			final class ActionListenerImplementation implements ActionListener {
				Timer timer;
				SchemeFunctionExecutor executor;
				ActionListenerImplementation( Procedure procedure ) {
					this.executor = new SchemeFunctionExecutor( procedure );
				}
				@Override
				public void actionPerformed(ActionEvent e) {
					Object result = executor.execute();					
					if ( Boolean.FALSE.equals( result ) ) {
						timer.stop();
					}
				}
			}

			@Override
			Object create( Pulsar pulsar, List<Object> args ) {
				if ( 2 <= args.size()  ) {
					int interval = SchemeUtils.toInteger(args.get(0));
					Procedure procedure = (Procedure)args.get(1);

					ActionListenerImplementation listener = new ActionListenerImplementation( procedure );
					Timer timer = new Timer( interval,  listener );
					listener.timer = timer;
					pulsar.addCleanupHook( new Runnable() {
						@Override
						public void run() {
							timer.stop();
						}
					});
					timer.start();
					return new ProcedureN() {
						public Object applyN(Object[] args) throws Throwable {
							timer.stop();
							return EmptyList.emptyList;
						};
					};
				} else {
					logInfo("WARNING : new 'timer was called but no argument was passed.");
					return EmptyList.emptyList;
				}

			}
		});

		register( "progress", new SchemeNewFactory() {
			@Override
			Object create(Pulsar pulsar, List<Object> args) {
				return new JProgressBar() {
//					@Override
//					public Dimension getPreferredSize() {
//						return new Dimension(
//								this.getParent().getSize().width,
//								super.getPreferredSize().height
//								);
//						
//					}
				};
			}
		});

		register( "newline", new SchemeNewFactory() {
			@Override
			Object create( Pulsar pulsar, List<Object> args ) {
				return FlawLayout.createNewLine();
			}
		});

		register( "slider", new SchemeNewFactory() {
			@Override
			Object create( Pulsar pulsar, List<Object> args ) {
				if ( args.size() == 5 ) {
					int min = ((Number)args.get(0)).intValue();
					int max = ((Number)args.get(1)).intValue();
					int minorTick = ((Number)args.get(2)).intValue();
					int majorTick = ((Number)args.get(3)).intValue();
					Procedure procedure = (Procedure) args.get(4);
					JSlider slider = new JSlider() {
						@Override
						public Dimension getPreferredSize() {
							Container parent = this.getParent();
							Insets i =((JComponent) parent ).getBorder().getBorderInsets( parent );
							return new Dimension(
									parent.getSize().width - (i.left + i.right ), 
									super.getPreferredSize().height
									);
							
						}
					};
					
					slider.setMaximum(max);
					slider.setMinimum(min);
					slider.setMajorTickSpacing( majorTick );
					slider.setMinorTickSpacing( minorTick );
					slider.setPaintTicks(true);
					slider.addChangeListener(new ChangeListener() {
						@Override
						public void stateChanged(ChangeEvent e) {
							try {
								procedure.applyN( new Object[] {IntNum.valueOf( slider.getValue() )  } );
							} catch (Throwable e1) {
								logError( "" , e1 );
							}
						}
					});
					return slider;
				} else {
					throw new RuntimeException( "(new 'slider min max tick-min tick-maj on-change)" );
				}
			}
		});
		
		register( "radio", new SchemeNewFactory() {
			@Override
			Object create( Pulsar pulsar, List<Object> args ) {
				return createSelectiveComponents( "'radio", args, new JRadioButtonFactory() );
			}

		});
		
		register( "check", new SchemeNewFactory() {
			@Override
			Object create( Pulsar pulsar, List<Object> args ) {
				return createSelectiveComponents( "'check", args, new JCheckBoxFactory() );
			}
		});
		register( "combo", new SchemeNewFactory() {
			
			@Override
			Object create( Pulsar pulsar, List<Object> args ) {
				Environment env = Environment.getCurrent();
				Language lang = Language.getDefaultLanguage();
				
				if ( 1 < args.size()  ) {
					Procedure procedure;
					{
						int last = args.size()-1;
						procedure = (Procedure) args.remove(last);
					}
					
					ActionListener actionListener = new ActionListener() {
						@Override
						public void actionPerformed(ActionEvent e) {
							try {
								Environment.setCurrent(env);
								Language.setCurrentLanguage(lang);
								JPulsarComboBox<PulsarListItem> comboBox = (JPulsarComboBox)e.getSource();
								PulsarListItem selectedItem = (PulsarListItem)comboBox.getSelectedItem();
								procedure.applyN( new Object[] { 
										true,
										IString.valueOf( selectedItem.getCaption() ),
										selectedItem.getUserObject(),
										e.getSource(), e  } ); 
							} catch (Throwable e1) {
								logError( "" , e1 );
							}
							
						}
					};
					
					PulsarListItem selectedItem = null ;
					Vector<PulsarListItem> items = new Vector<>();
					for ( Object o : args ) {
						if ( o instanceof Pair ) {
							Pair p = (Pair) o;
							String caption = SchemeUtils.toString( p.getCar());
							Object userObject = p.getCdr();
							items.add( new PulsarListItem(caption, userObject));
						} else if ( o instanceof IString ) {
							items.add( new PulsarListItem( SchemeUtils.toString(o), null));
						} else if ( o instanceof Symbol ) {
							if ( items.size() == 0  ) {
								throw new RuntimeException("error : no item was created");
							} else {
								selectedItem = items.get( items.size() - 1 );
							}
						} else {
							throw new RuntimeException( "An unsupported element type" );
						}
					}
					JPulsarComboBox<PulsarListItem> c = new JPulsarComboBox<>( items );
					c.setEnabled(true);
					if ( selectedItem != null )
						c.setSelectedItem( selectedItem );
					c.addActionListener( actionListener );
					
					return c; 
				} else {
					throw new RuntimeException( "(new 'combo [procedure] [caption | (cons caption . userData)]... " );
				}
			}
		});
	}

}
