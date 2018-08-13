package ats.pulsar;

import java.util.List;

abstract interface PulsableBuilder {
	abstract String getName();
	abstract List<Pulsable> create();	
}