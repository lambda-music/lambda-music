package lamu.utils.lib;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.lang.invoke.MethodHandles;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.logging.Level;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;

import gnu.lists.EmptyList;
import gnu.lists.IString;
import gnu.lists.Pair;
import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import lamu.lib.doc.LamuDocument;
import lamu.lib.evaluators.InvokablyRunnable;
import lamu.lib.evaluators.SchemeUtils;
import lamu.lib.log.Logger;
import lamu.lib.scheme.proc.MultipleNamedProcedureN;

public class PulsarGuiUtils {
    static final Logger LOGGER = Logger.getLogger( MethodHandles.lookup().lookupClass().getName() );
    static void logError(String msg, Throwable e) { LOGGER.log(Level.SEVERE, msg, e); }
    static void logInfo(String msg) { LOGGER.log(Level.INFO, msg); }
    static void logWarn(String msg) { LOGGER.log(Level.WARNING, msg); }
    static final int BORDER_SIZE = 10;

    private PulsarGuiUtils() {
    }

    public static void guiPack() {
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

        //      frame.pack();
        //      frame.pack();

        // SOMEHOW THIS MAKES THE FRAME SMALLER AND SMALLER WHEN DYNAMICALLY CREATING GROUPS.
        // SO I DISABLED IT. (Mon, 15 Jul 2019 10:08:05 +0900)
        // frame.pack();
        // frame.pack();
    }


    public static Component guiResolve( Container parent, Collection<String> path, boolean errorIfNotFound ) {
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

    static void guiRemoveByRef(Container parent, Collection<Object> argList ) {
        for ( Object o : argList ) {
            parent.remove( (Component)o );
        }
    }
    private static void guiRemoveByRef( Container parent, Component component) {
        parent.remove( component );
    }
    static void guiRemoveAll(Container parent ) {
        parent.removeAll();
    }


    public static Component guiRemoveByPath( Container parent, Collection<String> path ) {
        Component c = guiResolve( parent, path, false );
        parent.remove( c );
        return c;
    }


    public static Component guiGet( Container parent, Collection<String> path ) {
        return guiResolve( parent, path,  false );
    }
    public static List<Entry<String, Component>> guiListAll( Container parent ) {
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

    public static void guiAdd( Container parent, Component c, Object constraint ) {
        parent.add( c, constraint );
    }
    public static void guiAdd( Container parent, Component c  ) {
        parent.add( c  );
    }
    public static void guiInvokeLater( Procedure procedure, Object ... args  ) {
        SwingUtilities.invokeLater( InvokablyRunnable.createRunnableAndInvocable( procedure, args ) );
    }


    public static BorderLayout newLayout() {
        return new BorderLayout( BORDER_SIZE, BORDER_SIZE );
    }

    public static void guiFlowLayout(Container parent) {
        parent.setLayout( new FlawLayout( FlawLayout.LEFT, 2, 2 ) );
    }
    public static void guiBorderLayout(Container parent) {
        parent.setLayout( new BorderLayout( BORDER_SIZE,BORDER_SIZE ) );
    }
    public static void guiSpringLayout( Container parent ) {
        parent.setLayout( new SpringLayout() );
    }
    public static void guiBoxLayout( Container parent, int axis ) {
        parent.setLayout( new BoxLayout( parent, axis ) );
    }
    public static void guiGridBagLayout(Container parent) {
        parent.setLayout( new GridBagLayout() );
    }
    public static void guiGridLayout( Container parent, int row, int col, int hgap, int vgap  ) {
        logInfo( "gridLayout : " + row + " / " + col + " / " + hgap + " / " + vgap );
        parent.setLayout( new GridLayout( row, col, hgap, vgap ) );
    }

    //
    public static void guiLayout_auto( Container container, Object[] args ) {
        ArrayList<Object> argList = new ArrayList<>( Arrays.asList(args) );
        String    type = 0 < argList.size() ? SchemeUtils.schemeSymbolToJavaString( argList.remove(0) ) : "default";
        guiLayout( container, type, argList.toArray() );
    }
    public static void guiLayout( Container container, String type, Object ... args  ) {
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
    //  public void guiRepaint() {
    //      userPane.revalidate();
    //      userPane.repaint();
    //        // frame.pack();
    //  }


    public static Object guiBuild( Object[] args) {
        return guiBuild2( args );
    }

    @SuppressWarnings("unused")
    private Object guiBuild1( Object[] args) {
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
                String symbolName = SchemeUtils.schemeSymbolToJavaString( curr );

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
                    guiAdd( parent,(Component)
                        SchemeNewFactory.process(
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


    private static Object guiBuild2( Object[] args) {
        Container parent = (Container) args[0];

        if ( args[0] instanceof Container ) {
            parent = (Container) args[0]; 
        } else if ( args[0] instanceof EmptyList ) { 
            parent = null;
        } else if ( Boolean.FALSE.equals(args[0]) ) { 
            parent = null;
        } else {
            throw new RuntimeException( "An invalid parent object was specified. " );
        }

        String mode = null;
        for ( int i=1; i<args.length; i++ ) {
            try {
                Object curr = args[i];

                if ( "label".equals( mode ) ) {
                    guiAdd( parent, (Component)SchemeNewFactory.process(
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
                        String symbolName = SchemeUtils.schemeSymbolToJavaString( curr );
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
                        //                  if ( curr instanceof Pair  ) {
                        //                      Pair p = (Pair) curr;
                        //                      if ( "list".equals( mode ) ) {
                        //                          for ( Object e : p ) {
                        //                              guiAdd( parent, (Component) e );
                        //                          }
                        //                      } else {
                        //                          Object car = p.getCar();
                        //                          Object cdr = p.getCdr();
                        //                          
                        //                          if ( cdr instanceof Pair ) {
                        //                              guiAdd( parent, (Component)car, LayoutUtils.map2constraint( cdr ) );
                        //                          } else {
                        //                              guiAdd( parent, (Component)car, SchemeUtils.toString( cdr ) );
                        //                          }
                        //                      }
                        //                  } else {
                        //                      guiAdd( parent, (Component)curr );
                        //                  }
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

    public static void guiName( Container parent, String name ) {
        if ( parent instanceof JNamedPanel ) {
            ((JNamedPanel)parent).setNextComponentName( name );
        } else {
            logWarn( "WARNING guiName: the parent is not JNamedPanel" );
        }
    }
    public static void guiConstraint( Container parent, Object constraint ) {
        if ( parent instanceof JNamedPanel ) {
            ((JNamedPanel)parent).setNextConstraint( constraint );
        } else {
            logWarn( "WARNING guiConstraint: the parent is not JNamedPanel" );
        }
    }
    public static void guiProperty( Container parent, List<Object> propertyValues ) {
        if ( parent instanceof JNamedPanel ) {
            ((JNamedPanel)parent).setNextProperty( propertyValues );
        } else {
            logWarn( "WARNING guiProperty: the parent is not JNamedPanel" );
        }
    }
    public static void guiNextProcedure( Container parent, Procedure proc ) {
        if ( parent instanceof JNamedPanel ) {
            ((JNamedPanel)parent).setNextProcedure( proc );
        } else {
            logWarn( "WARNING guiProperty: the parent is not JNamedPanel" );
        }
    }
    public static void guiNextIndex( Container parent, int nextIndex ) {
        if ( parent instanceof JNamedPanel ) {
            ((JNamedPanel)parent).setNextIndex( nextIndex );
        } else {
            logWarn( "WARNING guiProperty: the parent is not JNamedPanel" );
        }
    }
    public static void guiNewline(Container parent ) {
        parent.add( FlawLayout.createNewLine() );
    }
    public static void guiValidate(Container parent) {
        parent.validate();
    }
    public static void guiInvalidate(Container parent) {
        parent.invalidate();
    }
    public static void guiRevalidate(Container parent) {
        parent.revalidate();
    }
    public static void guiRepaint(Container parent) {
        parent.repaint();;
    }

    public static void initScheme( Environment env ) {
        logInfo("PulsarGui#initStaticScheme=======================================");

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-pack" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                guiPack();
                return SchemeUtils.NO_RESULT;
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-new" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                //              try {
                //                  return SchemeNewFactory.process( pulsar, args);
                //              } catch ( Exception e ) {
                //                  logError("", e);
                //                  return null;
                //              }
                return SchemeNewFactory.process( args );
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-parent" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                if ( 1 <= args.length ) {
                    ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
                    Component component = (Component) SchemeUtils.schemeNullCheck(argList.pop());
                    Container parent = component.getParent();
                    return SchemeUtils.javaNullCheck( parent );
                } else {
                    throw new RuntimeException( 
                        "Invalid argument error\n"+
                        "usage : (gui-remove-by-name [parent] [name])" );
                }
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-remove-all" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                if ( 1 <= args.length ) {
                    ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
                    Container parent = (Container) SchemeUtils.schemeNullCheck(argList.pop());
                    guiRemoveAll(parent);
                    return SchemeUtils.NO_RESULT;
                } else {
                    throw new RuntimeException( 
                        "Invalid argument error\n"+
                        "usage : (gui-remove-by-name [parent] [name])" );
                }
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-remove-by-ref" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                if ( 1 < args.length ) {
                    ArrayDeque<Object> argList = new ArrayDeque<>( Arrays.asList(args) );
                    Container parent = (Container) SchemeUtils.schemeNullCheck(argList.pop());
                    guiRemoveByRef( parent, argList );
                    return SchemeUtils.NO_RESULT;
                } else {
                    throw new RuntimeException( 
                        "Invalid argument error\n"+
                        "usage : (gui-remove-by-name [parent] [name])" );
                }
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-remove-by-name" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                if ( 1 < args.length ) {
                    ArrayList<Object> argList = new ArrayList<>( Arrays.asList(args) );
                    Container parent = (Container) SchemeUtils.schemeNullCheck(argList.remove(0) );
                    List<String> path = SchemeUtils.convertList(argList, (o)->{
                        return SchemeUtils.toString(o);
                    });
                    Component c  = guiRemoveByPath( parent, path );
                    return SchemeUtils.javaNullCheck(c);
                } else {
                    throw new RuntimeException( 
                        "Invalid argument error\n"+
                        "usage : (gui-remove-by-name [parent] [name])" );
                }
            }
        });
        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-invoke-later" ) {
            @Override
            public Object applyN( Object[] args ) throws Throwable {
                guiInvokeLater( (Procedure) args[0], Arrays.copyOfRange(args, 1, args.length ) );
                return true;
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-get" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                if ( 1 == args.length ) {
                    Container parent = (Container) args[0];
                    List<Entry<String, Component>> list =  guiListAll(parent);
                    SchemeUtils.<Entry<String, Component>, Pair>convertList(list, (e)->{
                        String key = e.getKey();
                        return Pair.make(
                            ( key == null ? false : SchemeUtils.toSchemeSymbol( key ) ) , e.getValue() );
                    });
                    return Pair.makeList( (List)list );
                } else if ( 1 < args.length ) {
                    ArrayList<Object> argList = new ArrayList<>( Arrays.asList(args) );
                    Container parent = (Container) SchemeUtils.schemeNullCheck(argList.remove(0));
                    List<String> path = SchemeUtils.convertList(argList, (o)->{
                        return SchemeUtils.toString(o);
                    });

                    Component c  = guiGet( parent, path );
                    return SchemeUtils.javaNullCheck(c);
                } else {
                    throw new RuntimeException( 
                        "Invalid argument error\n"+
                        "usage : (gui-get [parent] [name ... ])" );
                }
            }
        });


        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-set-selected" ) {
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

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-set-text" ) {
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
         */
        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-set-user-object" ) {
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

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-get-user-object" ) {
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

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-build" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                return guiBuild( args );
            }
        });

        new LamuDocument(){{
            setCategory( "pulsar-gui-procedures" );
            setNames( "gui-build" );
            setParameterDescription( "" );
            setReturnValueDescription( "::Object" );
            setShortDescription( "an utility procedure to create a Swing component." );
            setLongDescription( ""
                + "This procedure utilizes process to build Swing components. "
                + "More description is comming now." );
        }};

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-newline" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo("newline-gui");
                if ( args.length == 0 )
                    guiNewline( null);
                else
                    guiNewline( (JComponent) args[0]);

                return SchemeUtils.NO_RESULT;
            }
        });

        //      SchemeUtils.defineVar( scheme, "gui-repaint" , new PulsarProcedureN("gui-repaint") {
        //          @Override
        //          public Object applyN(Object[] args) throws Throwable {
        ////                    logInfo("refresh-gui");
        //              guiRepaint();
        //              return Invokable.NO_RESULT;
        //          }
        //      });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-layout" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo( "gui-layout" );

                if ( args.length == 0 ) {
                    throw new IllegalArgumentException( "an insufficient number of arguments" );
                } else {
                    ArrayList<Object> argList = new ArrayList<>( Arrays.asList(args) );
                    Container container = 0 < argList.size() ? (Container) argList.remove(0) : null;
                    String    type      = 0 < argList.size() ? SchemeUtils.schemeSymbolToJavaString( argList.remove(0) ) : "default";
                    if ( container instanceof JFrame ){
                        container = ((JFrame)container).getContentPane();
                    }
                    guiLayout( container, type, argList.toArray() );

                    return container;
                }
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-gridbag-layout" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo( "gui-gridbag-layout" );
                if ( args.length < 1 ) {
                    throw new IllegalArgumentException( "an insufficient number of arguments" );
                } else {
                    guiGridBagLayout((Container)args[0]);
                    return SchemeUtils.NO_RESULT;
                }
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-spring-layout" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo( "gui-spring-layout" );
                if ( args.length < 1 )
                    throw new IllegalArgumentException( "an insufficient number of arguments" );

                guiSpringLayout((Container)args[0]);
                return SchemeUtils.NO_RESULT;
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-flow-layout" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo( "gui-flow-layout" );
                if ( args.length < 1 )
                    throw new IllegalArgumentException( "an insufficient number of arguments" );
                guiFlowLayout((Container)args[0]);
                return SchemeUtils.NO_RESULT;
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-put-constraint" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                logInfo("refresh-gui");
                JComponent userPane = (JComponent)args[0];
                SpringLayout springLayout = ((SpringLayout)userPane.getLayout());
                if ( args.length == 5 ) {
                    new SpringLayoutUtil(springLayout, userPane).putConstraint( args[1],args[2],args[3],args[4],args[5]  );
                } else if ( args.length == 4 ) {
                    new SpringLayoutUtil(springLayout, userPane).putConstraint( args[1],args[2], 5,     args[3], args[4] );
                } else {
                    throw new RuntimeException( "put-constraint has five parameters( constraint1 component1 pad constraint2 component2  )." );
                }
                guiRepaint(userPane);
                return SchemeUtils.NO_RESULT;
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-invalidate" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                guiInvalidate((Container) args[0] );
                return SchemeUtils.NO_RESULT;
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-validate" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                guiValidate((Container) args[0] );
                return SchemeUtils.NO_RESULT;
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-revalidate" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                guiRevalidate((Container) args[0] );
                return SchemeUtils.NO_RESULT;
            }
        });

        SchemeUtils.defineLambda( env, new MultipleNamedProcedureN( "gui-repaint" ) {
            @Override
            public Object applyN(Object[] args) throws Throwable {
                guiRepaint((Container) args[0] );
                return SchemeUtils.NO_RESULT;
            }
        });

        //      Kawapad.staticInitScheme( scheme );
    }

}
