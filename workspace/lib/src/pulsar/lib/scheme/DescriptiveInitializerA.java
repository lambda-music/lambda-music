package pulsar.lib.scheme;

public class DescriptiveInitializerA {
	public DescriptiveInitializerA() {
	}
	private String parameterDescription;
	private String returnValueDescription;
	private String shortDescription;
	private String longDescription;
	public String getParameterDescription() {
		return parameterDescription;
	}
	public void setParameterDescription(String parameterDescription) {
		this.parameterDescription = parameterDescription;
	}
	public String getReturnValueDescription() {
		return returnValueDescription;
	}
	public void setReturnValueDescription(String returnValueDescription) {
		this.returnValueDescription = returnValueDescription;
	}
	public String getShortDescription() {
		return shortDescription;
	}
	public void setShortDescription(String shortDescription) {
		this.shortDescription = shortDescription;
	}
	public String getLongDescription() {
		return longDescription;
	}
	public void setLongDescription(String longDescription) {
		this.longDescription = longDescription;
	}
}
