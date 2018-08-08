package nu.oka.metro;

import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

public interface MetroMasterLogic extends MetroLogic {
	public abstract Set<Entry<String,String>> optionalConnection();
	public abstract List<String> outputPortNameList();
	public abstract List<String> inputPortNameList();
	public abstract String clientName();
	public void initialize();

	public static abstract class Default implements MetroMasterLogic {
		protected MetroLogicHandle handle;
		@Override
		public void setLogicHandle(MetroLogicHandle handle) {
			this.handle = handle;
		}
	}

}

