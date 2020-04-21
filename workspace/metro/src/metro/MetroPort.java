package metro;

import org.jaudiolibs.jnajack.JackPort;
import org.jaudiolibs.jnajack.JackPortFlags;

import lamu.lib.evaluators.SchemeUtils;

public class MetroPort {
    boolean valid = true;
    Object name;
    JackPortFlags jackPortFlag;
    JackPort jackPort;
    public MetroPort() {
    }
    public MetroPort( Object name, JackPortFlags jackPortFlag, JackPort jackPort ) {
        if ( name == null )
            throw new NullPointerException();
        this.name = name;
        this.jackPortFlag = jackPortFlag;
        this.jackPort = jackPort;
    }
    public Object getName() {
        if ( ! valid )
            throw new IllegalStateException();
        return name;
    }
    public boolean isValid() {
        return valid;
    }
    
    @Override
    public String toString() {
        return SchemeUtils.anyToString( name );
    }
    @Override
    public boolean equals(Object obj) {
        if ( this == obj )
            return true;
        else if ( obj instanceof MetroPort ) {
            return name.equals( ((MetroPort)obj).name );
        } else {
            return false;
        }
    }
    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
