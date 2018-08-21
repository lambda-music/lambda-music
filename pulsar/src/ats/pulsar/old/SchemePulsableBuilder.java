package ats.pulsar.old;

import java.util.ArrayList;
import java.util.List;

import ats.pulsar.SchemeUtils;
import gnu.lists.Pair;
import gnu.mapping.Procedure;

class SchemePulsableBuilder implements PulsableBuilder {
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