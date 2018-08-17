package ats.pulsar;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import ats.metro.MetroLogic;
import ats.metro.MetroMidiEvent;
import ats.metro.MetroNoteEventBuffer;
import gnu.lists.AbstractSequence;
import gnu.lists.Pair;
import gnu.mapping.Procedure;

public class PulsarLogic extends MetroLogic.Default {
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

	static class SchemePulsable implements Pulsable {
		final double bars;
		final Procedure procedure;
		public SchemePulsable(double bars, Procedure procedure) {
			this.bars = bars;
			this.procedure = procedure;
		}

		@SuppressWarnings("unchecked")
		@Override
		public void pulse( MetroNoteEventBuffer buf ) {
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
					Map<String,Object> map = SchemeUtils.list2map(ep, (Integer idx)->{
//						double offset, int outputPortNo, int channel, int note, int velocity 
						switch ( idx ) {
							case 0:
								return "offset";
							case 1:
								return "portno";
							case 2:
								return "channel";
							case 3:
								return "note";
							case 4:
								return "velocity";
							default :
								return Integer.toString( idx );
						}
					});
					double offset    = map.containsKey( "offset"   ) ? SchemeUtils.toDouble( map.get("offset"    ) ) : 0.0d;  
					int outputPortNo = map.containsKey( "portno"   ) ? SchemeUtils.toInteger( map.get("portno"   ) ) : 1;
					int channel      = map.containsKey( "channel"  ) ? SchemeUtils.toInteger( map.get("channel"  ) ) : 0; 
					int note         = map.containsKey( "note"     ) ? SchemeUtils.toInteger( map.get("note"     ) ) : 63;  
					int velocity     = map.containsKey( "velocity" ) ? SchemeUtils.toInteger( map.get("velocity" ) ) : 63;
				
//					System.out.println( offset );
//					System.out.println( note );
					
					buf.noteShot(offset, outputPortNo, channel, note, velocity);
				}
			}
			buf.setLength( this.bars );
			
			// buf.noteShot(0, 1, 0, 73, 100 );
		}

		@Override
		public double getBars() {
			return bars;
		}
	}
	
	static class SchemePulsableBuilder implements PulsableBuilder {
		String name;
		String description;
		List<SchemePulsable> pulsableList;
		@Override
		public String getName() {
			return name;
		}

		public String getDescription() {
			return description;
		}

		public List<SchemePulsable> getPulsableList() {
			return pulsableList;
		}

		public SchemePulsableBuilder( String name, String description, List<Pair> pairs ) {
			super();
			this.name = name;
			this.description = description;
			this.pulsableList = new ArrayList<>();
			for ( int i=0; i< pairs.size();i++  ) {
				Pair p = pairs.get(i);
				pulsableList.add( new SchemePulsable( SchemeUtils.toDouble( p.getCar() ) , (Procedure) p.getCdr() ) );
			}
		}

		@Override
		public List<Pulsable> create() {
			return new ArrayList<>( this.pulsableList );
		}
	}
	
	static class SamplePulsableBuilder implements PulsableBuilder {
		String name;
		@Override
		public String getName() {
			return "default";
		}
		
		@Override
		public List<Pulsable> create() {
			ArrayList<Pulsable> result = new ArrayList<Pulsable>();
			
			result = new ArrayList<Pulsable>();
			{
				JavaPulse[][] arr = {
						{ new JavaPulse(73) },
						{},
						{},
						{ new JavaPulse(73) },
						{},
						{ new JavaPulse(true, 73,90) },
				};
				result.add( new JavaPulseList( arr, 7,2, 3 ) );
			}
			{
				JavaPulse[][] arr = {
					{ new JavaPulse(63,80) },
				};
				result.add( new JavaPulseList( arr, 4 ,2, 0 ) );
			}
			{
				JavaPulse[][] arr = {
					{ new JavaPulse(57,80) },
				};
				result.add( new JavaPulseList( arr, 1 ,2, 0 ) );
			}
			
			return result;
		}
	}

	////////////////////////////////////////////////////////////////////////////////////////
	//
	//
	//
	////////////////////////////////////////////////////////////////////////////////////////
	
	private boolean flag;
	private void notifyFlag() {
		this.flag = true;
	}

	List<Pulsable> pulsableList = new ArrayList<Pulsable>(); 
	{
		System.err.println("set-current-pulsable (from init)" );
		setCurrentPulsable( new SamplePulsableBuilder() );
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
	public void processInputMidiBuffer(List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
		out.addAll( in );
		System.err.println( "in.size()" + in.size() );
		System.err.println( "out.size()" + out.size() );
	}


	@Override
	public boolean processOutputNoteBuffer( MetroNoteEventBuffer buf ) {
		// System.out.println("Metro.logic.new MetroLogic() {...}.initBuffer()" );

		buf.humanize( 0.0d, 3 );

		double maxBars = 0.0d;
		for ( Pulsable pulsable : pulsableList ) {
			pulsable.pulse( buf );
			if ( maxBars < pulsable.getBars() )
				maxBars = pulsable.getBars();
		}
		
		// System.err.println( "maxBars" +  maxBars );
		buf.length( maxBars );

		
//		buf.noteShot( 0.0d  , 1 , 0, 57, 105 );
//		buf.noteShot( 0.02d , 1 , 0, 74, 127 );
////		buf.noteShot( 0.00d , 1 , 0, 74, 127 );
//		buf.noteShot( 0.2d  , 1 , 0, 73, 100 );
//		buf.noteShot( 0.4d  , 1 , 0, 73, 100 );
//		buf.noteShot( 0.6d  , 1 , 0, 73, 100 );
//		buf.noteShot( 0.8d  , 1 , 0, 73, 100 );
//		buf.length(     1.00d );

		if ( flag ) {
			handle.spawn( 0.1d, new MetroLogic.Default() {
				int cnt = 2;
				@Override
				public boolean processOutputNoteBuffer(MetroNoteEventBuffer buf) {
					//				buf.noteShot( 0.5d  , 1 , 0, 57, 127 );

					buf.noteShot( 0.0d  , 1 , 0, 63, 127 );
					buf.noteShot( 0.2d  , 1 , 0, 63, 80 );
					buf.noteShot( 0.4d  , 1 , 0, 63, 80 );
					buf.noteShot( 0.6d  , 1 , 0, 63, 80 );
					buf.noteShot( 0.8d  , 1 , 0, 63, 80 );
					buf.length(1.0d);
					return 0<cnt--;
				}
				@Override
				public void processInputMidiBuffer(List<MetroMidiEvent> in, List<MetroMidiEvent> out) {
				}
			});
			flag = false;
		}
		
		return true;
	}


}
