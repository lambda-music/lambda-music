package ats.pulsar.old;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import ats.metro.Metro;
import ats.metro.MetroLogic;
import ats.metro.MetroMidiEvent;
import ats.metro.MetroNoteEventBuffer;
import ats.metro.MetroNoteEventBufferSequence;
import ats.pulsar.SchemeUtils;
import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import gnu.mapping.Procedure;

public class PulsableLogic extends MetroLogic.Default {
	/*
	 * (list
	 *     (cons 1.0  (lambda() ... ))
	 *     (cons 2.0  (lambda() ... ))
	 *     (cons 2.5  (lambda() ... )))
	 */
	static ArrayList<Pair> parseListOfPairs( Pair pair ) {
		ArrayList<Pair> pairs = new ArrayList<Pair>();
		for ( Object pp : pair ) {
			pairs.add( (Pair)pp ); 
		}
		return pairs;
	}

	
	private boolean flag;
	private void notifyFlag() {
		this.flag = true;
	}

	List<Pulsable> pulsableList = new ArrayList<Pulsable>(); 
	{
		System.err.println("set-current-pulsable (from init)" );
//		setCurrentPulsable( new SamplePulsableBuilder() );
	}

	public PulsableLogic() {
		setCurrentPulsable( new SamplePulsableBuilder() );
	}

	public PulsableLogic ( String name, String description, List<Pair> pairs ) {
		setCurrentPulsable( new SchemePulsableBuilder( name, description, pairs ));
	}
	
	public void setCurrentPulsable( PulsableBuilder pulsableBuilder ) {
		System.err.println( "set current pulsable "  + pulsableBuilder.getName() );
		pulsableList.clear();
		pulsableList.addAll( pulsableBuilder.create() );
		
//		XXX
//		if ( getParent() != null )
//			getParent().clearSequences();
	}
	
	@Override
	public void processInputMidiBuffer(Metro metro, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
		out.addAll( in );
		System.err.println( "in.size()" + in.size() );
		System.err.println( "out.size()" + out.size() );
	}


	@Override
	public boolean processOutputNoteBuffer( Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf ) {
		// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );

		buf.humanize( 0.0d, 3 );

		double maxBars = 0.0d;
		for ( Pulsable pulsable : pulsableList ) {
			pulsable.pulse( metro,sequence, buf );
			if ( maxBars < pulsable.getBarLength() )
				maxBars = pulsable.getBarLength();
		}
		
		buf.length( maxBars );

		
		if ( flag ) {
			handle.spawn( "LogcChildXXX", 0.1d, new MetroLogic.Default() {
				int cnt = 2;
				@Override
				public boolean processOutputNoteBuffer(Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf) {
					//				buf.noteShot( 0.5d  , 1 , 0, 57, 127 );

					buf.noteHit( 0.0d  , 1 , 0, 63, 127 );
					buf.noteHit( 0.2d  , 1 , 0, 63, 80 );
					buf.noteHit( 0.4d  , 1 , 0, 63, 80 );
					buf.noteHit( 0.6d  , 1 , 0, 63, 80 );
					buf.noteHit( 0.8d  , 1 , 0, 63, 80 );
					buf.length(1.0d);
					return 0<cnt--;
				}
				@Override
				public void processInputMidiBuffer(Metro metro, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
				}
			});
			flag = false;
		}
		
		return true;
	}

	@SuppressWarnings("unchecked")
	public static void scheme2buf( Procedure procedure, MetroNoteEventBuffer buf) {
		AbstractSequence<Object> pattern ;
		try {
			pattern = (AbstractSequence<Object>) procedure.applyN( new Object[] {} );
		} catch (Throwable e) {
			e.printStackTrace();
			pattern = null;
		}
	
		if ( pattern != null ) {
			for ( Iterator<Object> i = pattern.iterator(); i.hasNext(); ) {
				Pair ep = (Pair)i.next();
				Map<String,Object> map = SchemeUtils.list2map(ep,
						(Object type)->{
							switch ( SchemeUtils.symbolToString(type)) {
								case "on":
								case "off":
								case "hit":
									return (Integer idx)->{
										//	double offset, int outputPortNo, int channel, int note, int velocity 
										switch ( idx ) {
											case 0:
												return "type";
											case 1:
												return "port";
											case 2:
												return "channel";
											case 3:
												return "offset";
											case 4:
												return "note";
											case 5:
												return "velocity";
											case 6:
												return "length";
											default :
												throw new RuntimeException( "cannot omit the key object." );
										}
									};
								case "bar":
									return (Integer idx)->{
										switch ( idx ) {
											case 0:
												return "type";
											case 1:
												return "length";
											default :
												throw new RuntimeException( "cannot omit the key object." );
										}
									};
								default : {
									throw new RuntimeException( "cannot omit the key object." );
								}
							}
						});
	
				String type      = map.containsKey( "type"     ) ? SchemeUtils.symbolToString(  map.get("type"      ) ) : "";
				int port         = map.containsKey( "port"     ) ? SchemeUtils.toInteger( map.get("port"      ) ) : 1;
				int channel      = map.containsKey( "channel"  ) ? SchemeUtils.toInteger( map.get("channel"   ) ) : 0; 
				double offset    = map.containsKey( "offset"   ) ? SchemeUtils.toDouble(  map.get("offset"    ) ) : 0.0d;  
				int note         = map.containsKey( "note"     ) ? SchemeUtils.toInteger( map.get("note"      ) ) : 63;  
				int velocity     = map.containsKey( "velocity" ) ? SchemeUtils.toInteger( map.get("velocity"  ) ) : 63;
				double length    = map.containsKey( "length"   ) ? SchemeUtils.toInteger( map.get("length"    ) ) : -1;
	
				switch ( type ) {
					case  "hit" : {
						buf.noteHit(offset, port, channel, note, velocity, length );
						break;
					}
					case  "on" : {
						buf.noteOn(offset, port, channel, note, velocity );
						break;
					}
					case  "off" : {
						buf.noteOff(offset, port, channel, note, velocity );
						break;
					}
					case  "bar" : {
						if ( length < 0 )
							length = 1.0d;
						buf.setLength( length );
						break;
					}
					default : {
						Logger.getLogger(Metro.class.getName()).log(Level.WARNING, null, "unknown type (" +  type + ")" );
					}
				}
			}
		}
		// buf.setLength( this.bars );
		// buf.noteShot(0, 1, 0, 73, 100 );
	}
}
