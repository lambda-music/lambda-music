package nu.oka.metro;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

public interface MetroMasterLogic extends MetroLogic {
	public default Set<Entry<String,String>> optionalConnection() {
		return new LinkedHashMap<String,String>().entrySet();
	}
	public default List<String> outputPortNameList() {
		return new ArrayList<String>();
	}
	public default List<String> inputPortNameList() {
		return new ArrayList<String>();
	}
	public abstract String clientName();
	public void initialize();

	public static abstract class Default implements MetroMasterLogic {
		protected Metro parent;
		@Override
		public Metro getParent() {
			return this.parent;
		}
		@Override
		public void setParent(Metro parent) {
			this.parent = parent;
		}

		protected MetroLogicHandle handle;
		@Override
		public void setLogicHandle(MetroLogicHandle handle) {
			this.handle = handle;
		}
	}

}

