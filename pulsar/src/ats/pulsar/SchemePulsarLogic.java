package ats.pulsar;

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
import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import gnu.mapping.Procedure;

public class SchemePulsarLogic extends MetroLogic.Default {

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

	private Procedure procedure;
	public SchemePulsarLogic ( Procedure procedure ) {
		this.procedure = procedure;
	}
	
	@Override
	public void processInputMidiBuffer(Metro metro, List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
		// out.addAll( in ); TODO ******************************
		System.err.println( "in.size()" + in.size() );
		System.err.println( "out.size()" + out.size() );
	}


	@Override
	public boolean processOutputNoteBuffer( Metro metro, MetroNoteEventBufferSequence sequence, MetroNoteEventBuffer buf ) {
		// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );
		buf.humanize( 0.0d, 3 );
		scheme2buf(metro, sequence, procedure, buf);
		return true;
	}

	@SuppressWarnings("unchecked")
	public static boolean scheme2buf( Metro metro, MetroNoteEventBufferSequence sequence, Procedure procedure, MetroNoteEventBuffer buf) {
		AbstractSequence<Object> pattern ;
		try {
			pattern = (AbstractSequence<Object>) procedure.applyN( new Object[] {} );
		} catch (Throwable e) {
			e.printStackTrace();
			pattern = null;
		}

		/*
		class Processor {
		}
		class NoteProcessor extends Processor {
		}
		class State {
			Processor processor = null;
		}
		State state = new State();
		*/
	
		boolean result = true;
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
											case 0: return "type";
											case 1: return "port";
											case 2: return "channel";
											case 3: return "offset";
											case 4: return "note";
											case 5: return "velocity";
											case 6: return "length";
											default : throw new RuntimeException( "cannot omit the key object." );
										}
									};
								case "bar":
									return (Integer idx)->{
										switch ( idx ) {
											case 0: return "type";
											case 1: return "length";
											default : throw new RuntimeException( "cannot omit the key object." );
										}
									};
								case "end":
									return (Integer idx)->{
										switch ( idx ) {
											case 0: return "type";
											case 1: return "when";
											default : throw new RuntimeException( "cannot omit the key object." );
										}
									};
								case "start-new":
									return (Integer idx)->{
										switch ( idx ) {
											case 0: return "type";
											case 1: return "offset";
											case 2: return "name";
											case 3: return "procedure";
											default : throw new RuntimeException( "cannot omit the key object." );
										}
									};
								default : {
									throw new RuntimeException( "cannot omit the key object." );
								}
							}
						});
	
				String type      = map.containsKey( "type"     ) ? SchemeUtils.symbolToString(  map.get("type"      ) ) : "";
	
				switch ( type ) {
					case  "hit" : {
						int port         = map.containsKey( "port"     ) ? SchemeUtils.toInteger(       map.get("port"      ) ) : 1;
						int channel      = map.containsKey( "channel"  ) ? SchemeUtils.toInteger(       map.get("channel"   ) ) : 0; 
						double offset    = map.containsKey( "offset"   ) ? SchemeUtils.toDouble(        map.get("offset"    ) ) : 0.0d;  
						int note         = map.containsKey( "note"     ) ? SchemeUtils.toInteger(       map.get("note"      ) ) : 63;  
						int velocity     = map.containsKey( "velocity" ) ? SchemeUtils.toInteger(       map.get("velocity"  ) ) : 63;
						double length    = map.containsKey( "length"   ) ? SchemeUtils.toInteger(       map.get("length"    ) ) : -1;

						buf.noteHit(offset, port, channel, note, velocity, length );
						break;
					}
					case  "on" : {
						int port         = map.containsKey( "port"     ) ? SchemeUtils.toInteger(       map.get("port"      ) ) : 1;
						int channel      = map.containsKey( "channel"  ) ? SchemeUtils.toInteger(       map.get("channel"   ) ) : 0; 
						double offset    = map.containsKey( "offset"   ) ? SchemeUtils.toDouble(        map.get("offset"    ) ) : 0.0d;  
						int note         = map.containsKey( "note"     ) ? SchemeUtils.toInteger(       map.get("note"      ) ) : 63;  
						int velocity     = map.containsKey( "velocity" ) ? SchemeUtils.toInteger(       map.get("velocity"  ) ) : 63;
						double length    = map.containsKey( "length"   ) ? SchemeUtils.toInteger(       map.get("length"    ) ) : -1;

						buf.noteOn(offset, port, channel, note, velocity );
						break;
					}
					case  "off" : {																														
						int port         = map.containsKey( "port"     ) ? SchemeUtils.toInteger(       map.get("port"      ) ) : 1;
						int channel      = map.containsKey( "channel"  ) ? SchemeUtils.toInteger(       map.get("channel"   ) ) : 0; 
						double offset    = map.containsKey( "offset"   ) ? SchemeUtils.toDouble(        map.get("offset"    ) ) : 0.0d;  
						int note         = map.containsKey( "note"     ) ? SchemeUtils.toInteger(       map.get("note"      ) ) : 63;  
						int velocity     = map.containsKey( "velocity" ) ? SchemeUtils.toInteger(       map.get("velocity"  ) ) : 63;
						double length    = map.containsKey( "length"   ) ? SchemeUtils.toInteger(       map.get("length"    ) ) : -1;
						
						buf.noteOff(offset, port, channel, note, velocity );
						break;
					}
					case  "bar" : {
						double length    = map.containsKey( "length"   ) ? SchemeUtils.toInteger(       map.get("length"    ) ) : -1;
						
						if ( length < 0 )
							length = 1.0d;
						buf.setLength( length );
						break;
					}
//					case  "end" : {
//						double    offset    = map.containsKey( "offset"   ) ? SchemeUtils.toDouble(        map.get("offset"    ) ) : 0.0d;  
//						buf.end( offset );
//
//						break;
//					}
					case  "start-new" : {
						double    offset    = map.containsKey( "offset"   ) ? SchemeUtils.toDouble(        map.get("offset"    ) ) : 0.0d;  
						String    name      = map.containsKey( "name"     ) ? SchemeUtils.toString(        map.get( "name"      ) ) : "";
						Procedure proc      = (Procedure) map.get( "procedure" );
						SchemePulsarLogic logic = new SchemePulsarLogic( procedure );
						metro.putLogicSync( name, logic, sequence, offset );

						break;
					}
					default : {
						Logger.getLogger(Metro.class.getName()).log(Level.WARNING, null, "unknown type (" +  type + ")" );
					}
				}
			} // end of the loop
		}
		// buf.setLength( this.bars );
		// buf.noteShot(0, 1, 0, 73, 100 );
		return result;
	}
}
