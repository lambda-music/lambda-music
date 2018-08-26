
package org.jaudiolibs.examples;

import java.util.ServiceLoader;

import org.jaudiolibs.audioservers.AudioServerProvider;
import org.jaudiolibs.audioservers.ext.Device;

/**
 *
 * @author Neil C Smith
 */
public class DeviceIteration {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        for (AudioServerProvider provider : ServiceLoader.load(AudioServerProvider.class)) {
            System.out.print( "Found library : " + provider.getLibraryName() );
            System.out.print( "==============================================" );
            System.out.print( "Devices" );
            System.out.print( "----------------------------------------------" );
            for (Device dev : provider.findAll(Device.class)) {
                System.out.print( dev.getName() + " (inputs: " + dev.getMaxInputChannels() + ", outputs: " + dev.getMaxOutputChannels() + ")" );
                
            }
        }
    }
}
