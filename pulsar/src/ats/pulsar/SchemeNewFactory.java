package ats.pulsar;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JSlider;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import ats.pulsar.lib.FlawLayout;
import gnu.expr.Language;
import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.ProcedureN;
import gnu.math.IntNum;

public abstract class SchemeNewFactory {
	private static final Map<String, SchemeNewFactory> map = new HashMap<>();
	abstract Object create( List<Object> args );
	static void register( String key, SchemeNewFactory factory ) {
		if ( map.containsKey(key))
			throw new RuntimeException( "the key was already registered (" + key + ")" );

		map.put(key, factory );
	}

	static void logError( String msg, Throwable e ) {
        Logger.getLogger(Pulsar.class.getName()).log(Level.SEVERE, msg, e);
	}
	static void logInfo( String msg ) {
//        Logger.getLogger(Pulsar.class.getName()).log(Level.INFO, msg);
		System.err.println( msg );
	}

	public static Object process( Object ... args ) {
		ArrayList<Object> arguments = new ArrayList<>( Arrays.asList( args ) );
		Object key = arguments.remove(0);
		if ( key == null )
			throw new RuntimeException("key is not specified");
		
		String keyString = SchemeUtils.anyToString(key);
		SchemeNewFactory factory = map.get(keyString);
		if ( factory == null ) {
			throw new RuntimeException("unknown object type (" + keyString + ")" );
		}
		Object result = factory.create( arguments );

		return result;
	}

	abstract class SchemeActionListener implements ActionListener {
		private final Procedure procedure;
		private final Environment env;
		private final Language lang;

		private SchemeActionListener(Procedure procedure ) {
			this.procedure = procedure;
			this.env = Environment.getCurrent();
			this.lang = Language.getDefaultLanguage();
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			execute(e);
		}
		
		abstract void process( Procedure procedure ) throws Throwable;
		
		public void execute( ActionEvent e ) {
			try {
				Environment.setCurrent(env);
				Language.setCurrentLanguage(lang);
				process( this.procedure );
			} catch (Throwable e1) {
				logError( "" , e1 );
			}
		}
	}

	static abstract class ComponentFactory {
		abstract Component create( ActionListener l, String caption );
	}

	static class JRadioButtonFactory extends ComponentFactory {
		ButtonGroup group = new ButtonGroup();
		@Override
		Component create(ActionListener l, String caption) {
			JRadioButton r = new JRadioButton( caption );
			r.setActionCommand( caption );
			r.addActionListener( l );
			group.add( r );
			return r;
		}
	}
	static class JCheckBoxFactory extends ComponentFactory {
		@Override
		Component create(ActionListener l, String caption) {
			JCheckBox r = new JCheckBox( caption );
			r.setActionCommand( caption );
			r.addActionListener( l );
			return r;
		}
	}
	
	static Object createSelectiveComponents( String type, List<Object> args, ComponentFactory f ) {
		
		if ( 1 < args.size()  ) {
			Environment env = Environment.getCurrent();
			Language lang = Language.getDefaultLanguage();
			Procedure procedure;
			{
				int last = args.size() - 1;
				procedure = (Procedure) args.get( last );
				args.remove(last);
			}
			
			ActionListener listener = new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					try {
						Environment.setCurrent(env);
						Language.setCurrentLanguage(lang);
						// System.out.println( e.getSource() );
						procedure.applyN( new Object[] { 
								((AbstractButton)e.getSource()).isSelected(),
								IString.valueOf( e.getActionCommand() ),
								e.getSource(), e  } ); 
					} catch (Throwable e1) {
						logError( "" , e1 );
					}
				}
			};
			
			
			ButtonGroup group = new ButtonGroup();
			List<Object> result = new ArrayList();
			for ( Object e : args ) {
				String caption = SchemeUtils.anyToString(e);
				result.add( f.create(listener, caption) );
			}
			return Pair.makeList( result );
		} else {
			throw new RuntimeException( "new " + type + "[ caption, caption, ..., procedure] " + args.size() );
		}
	}
	
	static {
		register( "label", new SchemeNewFactory() {
			@Override
			Object create(List<Object> args ) {
				if ( 0<args.size()  ) {
					return new JLabel( SchemeUtils.toString( args.get(0) ) );
				} else {
					return EmptyList.emptyList;
				}
			}
		});
		register( "panel", new SchemeNewFactory() {
			@Override
			Object create(List<Object> args ) {
				LayoutManager m;
				if ( args.size == 0 ) {
					m = new BorderLayout( Pulsar.BORDER_SIZE, Pulsar.BORDER_SIZE );
				} else {
					switch ( SchemeUtils.symbolToString( args.get(0) ) ) {
						
					}
					
				}
				return new JNamedPanel();
			}
		});

		register( "button", new SchemeNewFactory() {
			@Override
			Object create( List<Object> args ) {
				if ( args.size() == 2 ) {
					String caption = ((IString) args.get(0)).toString();
					Procedure procedure = (Procedure) args.get(1);

					Environment env = Environment.getCurrent();
					Language lang = Language.getDefaultLanguage();
					{
						JButton button = new JButton( caption );
						button.addActionListener( new ActionListener() {
							@Override
							public void actionPerformed(ActionEvent e) {
								try {
									Environment.setCurrent(env);
									Language.setCurrentLanguage(lang);
									procedure.applyN( new Object[] { e } );
								} catch (Throwable e1) {
									logError( "" , e1 );
								}
							}
						});
						return button;
					}
				} else {
					throw new RuntimeException( "new 'button has two parameters( caption proc )." + args.size() );
				}
			}
		});

		register( "timer", new SchemeNewFactory() {
			final class ActionListenerImplementation extends SchemeActionListener {
				Timer timer; 
				ActionListenerImplementation(Procedure procedure ) {
					super( procedure );
				}
				@Override
				void process(Procedure procedure) throws Throwable {
					Object result = procedure.applyN( new Object[] { } );					
					if ( Boolean.FALSE.equals( result ) ) {
						timer.stop();
					}
				}
			}

			@Override
			Object create( List<Object> args ) {
				if ( 2 <= args.size()  ) {
					int interval = SchemeUtils.toInteger(args.get(0));
					Procedure procedure = (Procedure)args.get(1);

					ActionListenerImplementation listener = new ActionListenerImplementation(procedure );
					Timer timer = new Timer( interval,  listener );
					listener.timer = timer;
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
			Object create(List<Object> args) {
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
			Object create( List<Object> args ) {
				return FlawLayout.createNewLine();
			}
		});

		register( "slider", new SchemeNewFactory() {
			@Override
			Object create( List<Object> args ) {
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
			Object create( List<Object> args ) {
				return createSelectiveComponents( "'radio", args, new JRadioButtonFactory() );
			}

		});
		
		register( "check", new SchemeNewFactory() {
			@Override
			Object create( List<Object> args ) {
				return createSelectiveComponents( "'check", args, new JCheckBoxFactory() );
			}
		});
	}

}
