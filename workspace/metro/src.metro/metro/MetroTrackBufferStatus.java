package metro;

/**
 * A class which stores the relative location from the current playing position
 * of a specific {@link MetroEventBuffer} in a {@link MetroBufferedSequence}.
 */
public class MetroTrackBufferStatus {
	/**
	 * The cursor position in the current playing buffer.
	 */
	double currentPosition =-1;

	/**
	 * The sequence number of the current playing buffer.
	 */
	long currentBufferSeqNo=-1;
	
	
	/**
	 * The position of the current playing buffer. This should always be zero.
	 */
	double currentBufferPosition=-1;
	
	/**
	 * The length of the current playing buffer.
	 */
	double currentBufferLength=-1;

	/**
	 * The sequence number of the specified buffer.
	 */
	long bufferSeqNo =-1;

	/**
	 * The relative location of the head of the specified buffer.
	 */
	double bufferPosition=-1;
	
	/**
	 * The length of the specified buffer. This field may not be used but is there for symmetricity.
	 */
	double bufferLength=-1;

	
	
	
	
	/**
	 * @return the trackPosition
	 */
	public double getCurrentPosition() {
		return currentPosition;
	}

	/**
	 * @param trackPosition the trackPosition to set
	 */
	public void setCurrentPosition(double trackPosition) {
		this.currentPosition = trackPosition;
	}

	/**
	 * @return the currentBufferSeqNo
	 */
	public long getCurrentBufferSeqNo() {
		return currentBufferSeqNo;
	}

	/**
	 * @param currentBufferSeqNo the currentBufferSeqNo to set
	 */
	public void setCurrentBufferSeqNo(long currentBufferSeqNo) {
		this.currentBufferSeqNo = currentBufferSeqNo;
	}

	/**
	 * @return the currentBufferPosition
	 */
	public double getCurrentBufferPosition() {
		return currentBufferPosition;
	}

	/**
	 * @param currentBufferPosition the currentBufferPosition to set
	 */
	public void setCurrentBufferPosition(double currentBufferPosition) {
		this.currentBufferPosition = currentBufferPosition;
	}

	/**
	 * @return the currentBufferLength
	 */
	public double getCurrentBufferLength() {
		return currentBufferLength;
	}

	/**
	 * @param currentBufferLength the currentBufferLength to set
	 */
	public void setCurrentBufferLength(double currentBufferLength) {
		this.currentBufferLength = currentBufferLength;
	}

	/**
	 * @return the bufferSeqNo
	 */
	public long getBufferSeqNo() {
		return bufferSeqNo;
	}

	/**
	 * @param bufferSeqNo the bufferSeqNo to set
	 */
	public void setBufferSeqNo(long bufferSeqNo) {
		this.bufferSeqNo = bufferSeqNo;
	}
	public void setBufferSeqNo( MetroEventBuffer buffer ) {
		this.bufferSeqNo = buffer.getSeqNo();
	}

	/**
	 * @return the bufferPosition
	 */
	public double getBufferPosition() {
		return bufferPosition;
	}

	/**
	 * @param bufferPosition the bufferPosition to set
	 */
	public void setBufferPosition(double bufferPosition) {
		this.bufferPosition = bufferPosition;
	}
	/**
	 * @return the bufferLength
	 */
	public double getBufferLength() {
		return bufferLength;
	}
	/**
	 * @param bufferLength the bufferLength to set
	 */
	public void setBufferLength(double bufferLength) {
		this.bufferLength = bufferLength;
	}
	public void setBufferLength( MetroEventBuffer buffer ) {
		this.bufferLength = buffer.getLength();
	}
	

}
