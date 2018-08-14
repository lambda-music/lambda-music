package ats.pulsar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ats.metro.MetroNoteEventBuffer;

public class JavaPulseList implements Pulsable {
	static List<Set<JavaPulse>> asList( JavaPulse[][] list, int repeatCount ) {
		List<Set<JavaPulse>> result = new ArrayList<Set<JavaPulse>>();
		for ( int i=0; i<repeatCount; i++) {
			for ( JavaPulse[] set : list ) {
				result.add( new HashSet<JavaPulse>( Arrays.asList( set )  ) );
			}
		}
		System.out.println( result  );
		return result;
	}
	List<Set<JavaPulse>> pulseSetList;
	double bars;
	public JavaPulseList(List<Set<JavaPulse>> pulseSetList, double bars, int rotate ) {
		super();
		this.pulseSetList = pulseSetList;
		this.bars = bars;
		this.rotate( rotate );
	}
	public JavaPulseList(JavaPulse[][] pulses, double bars, int rotate ) {
		super();
		this.pulseSetList = asList( pulses, 1 );
		this.bars = bars;
		this.rotate( rotate );
	}
	public JavaPulseList(JavaPulse[][] tuplets, int beats, double bars, int rotate ) {
		super();
		this.pulseSetList = asList( tuplets, beats );
		this.bars = bars;
		this.rotate( rotate );
	}
	public List<Set<JavaPulse>> getPulseSetList() {
		return pulseSetList;
	}
	@Override
	public double getBars() {
		return bars;
	}
	public JavaPulseList rotate( int distance ) {
		Collections.rotate( this.pulseSetList, distance );
		return this;
	}
	public int centerPulseIndex() {
		// Returns the first pulse index where the centerPulse is set to `true`.
		for ( int i=0; i<this.pulseSetList.size(); i++ ) {
			Set<JavaPulse> pulseSet = this.pulseSetList.get(i);
			for ( JavaPulse pulse : pulseSet ) {
				if ( pulse.centerPulse ) {
					return i;
				}
			}
		}
		// Otherwise returns zero as its default value. 
		return 0;
	}
	
	@Override
	public void pulse( MetroNoteEventBuffer buf ) {
		JavaPulseList pattern = this;
		List<Set<JavaPulse>> pulseSetList = pattern.getPulseSetList();
		double bars = (double)pattern.getBars();
		double pulseLength = bars / pulseSetList.size();
		double pulseIndex = 0;
		for ( Set<JavaPulse> pulseSet : pulseSetList ) {
			double pulsePos = ( pulseIndex * pulseLength );
			for ( JavaPulse note : pulseSet ) {
				buf.noteShot( pulsePos, note.port , note.channel, note.note, note.velocity );
			}
			pulseIndex ++;
		}
	}
}