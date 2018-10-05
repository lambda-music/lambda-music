package ats.pulsar.editor.lib;

public class LazyGroupedUndoManager extends GroupedUndoManager {
	@Override
	public void startGroup() {
	}
	@Override
	public void setSuspended(boolean suspended) {
	}
	@Override
	public boolean isSuspended() {
		return false;
	}
}
