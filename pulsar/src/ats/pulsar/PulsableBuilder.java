package ats.pulsar;

import java.util.List;

public abstract interface PulsableBuilder {
	abstract String getName();
	abstract List<Pulsable> create();	
}