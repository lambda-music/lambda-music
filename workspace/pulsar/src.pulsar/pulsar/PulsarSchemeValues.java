package pulsar;

import lamu.lib.kawautils.SchemeValues;
import metro.MetroTrackMode;

public class PulsarSchemeValues {

    /**
     * Returns a specified {@link MetroTrackMode} object from any object value.
     * The value is converted to a string value by {@link SchemeValues#anyToString(Object)} method,
     * then convert to the {@link MetroTrackMode} value. 
     * 
     * @param  any object value which represents the {@link MetroTrackMode} value. 
     * @return
     *    a MetroTrackMode object or null if the input value is null. 
     */
    public static MetroTrackMode fromObject( Object s ) {
        if ( s == null )
            return null;
        else
            return MetroTrackMode.fromString( SchemeValues.anyToString( s ));
    }

}
