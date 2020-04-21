package pulsar;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.regex.Pattern;

import gnu.mapping.Environment;
import gnu.mapping.Procedure;
import gnu.mapping.Values;
import gnu.math.DFloNum;
import lamu.lib.MersenneTwisterFast;
import lamu.lib.doc.LamuDocument;
import lamu.lib.evaluators.Invokable;
import lamu.lib.evaluators.SchemeUtils;
import lamu.lib.scheme.proc.MultipleNamedProcedure0;
import lamu.lib.scheme.proc.MultipleNamedProcedure1;
import lamu.lib.scheme.proc.MultipleNamedProcedureN;
import metro.MetroTrack;
import pulsar.PulsarLib.PulsarLibImplementation.PulsarProceduralDescriptiveDoc;

public class PulsarLib_Procs {

    public static final Procedure getTrackPositionProc = new GetTrackPositionProc(new String[] { "get-track-position", "gettp" });
    public static final class GetTrackPositionProc extends MultipleNamedProcedure1 {
        public GetTrackPositionProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1( Object arg1 ) throws Throwable {
            if ( Boolean.FALSE.equals( arg1 ) ) { 
                return Boolean.FALSE;
            } else {
                MetroTrack track = ((MetroTrack)arg1);
                double position=-1d;
                synchronized ( track.getMetroTrackLock() ) {
                    position = track.getTrackPosition();
                }
                
                return SchemeUtils.toSchemeNumber( position );
            }
        }
    }

    public static final GetTrackPositionBean getTrackPositionBean = new GetTrackPositionBean();
    public static final class GetTrackPositionBean extends PulsarProceduralDescriptiveDoc {
        {
            setCategory( Pulsar.DOCS_ID );
            setNames( "get-track-position", "gettp" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| gets the current position of the given track." );
            setLongDescription( ""
                                + "" 
                             );
        }
    }

    public static final PrintStackTraceProc printStackTraceProc = new PrintStackTraceProc(new String[] { "print-stack-trace" });
    public static final class PrintStackTraceProc extends MultipleNamedProcedure0 {
        public PrintStackTraceProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            PrintStream out = null;
            ByteArrayOutputStream bout = null;
            try {
                bout = new ByteArrayOutputStream();
                out = new PrintStream( bout );
                new Throwable().printStackTrace( out );
                out.flush();
                String value = new String( bout.toByteArray(), Charset.defaultCharset() );
                value = Pattern.compile( "^\\s+", Pattern.MULTILINE ).matcher( value ).replaceAll( "" );
                return SchemeUtils.toSchemeString( value ) ;
            } finally {
                if ( bout != null )
                    bout.close();
                if ( out != null )
                    out.close();
            }
        }
    }

    public static final PrintStackTraceBean printStackTraceBean = new PrintStackTraceBean();
    public static final class PrintStackTraceBean extends PulsarProceduralDescriptiveDoc {
        {
            setCategory( Pulsar.DOCS_ID );
            setNames( "print-stack-trace" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| returns the current stack trace as a string. " );
            setLongDescription( ""
                                + "" 
                             );
        }
    }

    public static final DisplayWarnProc displayWarnProc = new DisplayWarnProc(new String[] { "display-warn" });
    public static final class DisplayWarnProc extends MultipleNamedProcedure1 {
        public DisplayWarnProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg1) throws Throwable {
            System.err.print( arg1 );
            return Values.empty;
        }
    }

    public static final DisplayWarnBean displayWarnBean = new DisplayWarnBean();
    public static final class DisplayWarnBean extends PulsarProceduralDescriptiveDoc {
        {
            setCategory( Pulsar.DOCS_ID );
            setNames( "display-warn" );
            setParameterDescription( "any" );
            addParameter( 0, "value", "any", null, false , "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| output the specified value to the standard error stream. " );
            setLongDescription( ""
                                + "" 
                             );
        }
    }

    public static final NewlineWarnProc newlineWarnProc = new NewlineWarnProc(new String[] { "newline-warn" });
    public static final class NewlineWarnProc extends MultipleNamedProcedure0 {
        public NewlineWarnProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply0() throws Throwable {
            System.err.println();
            return Values.empty;
        }
    }

    public static final NewlineWarnBean newlineWarnBean = new NewlineWarnBean();
    public static final class NewlineWarnBean extends PulsarProceduralDescriptiveDoc {
        {
            setCategory( Pulsar.DOCS_ID );
            setNames( "newline-warn" );
            setParameterDescription( "" );
            setReturnValueDescription( "::void" );
            setShortDescription( "||<name/>|| output a line terminator to the standard error stream. " );
            setLongDescription( ""
                                + "" 
                             );
        }
    }

    public static final TypeofProc typeofProc = new TypeofProc(new String[] { "typeof" });
    public static final class TypeofProc extends MultipleNamedProcedureN {
        public TypeofProc(String[] names) {
            super(names);
        }

        public Object applyN(Object[] args) throws Throwable {
            if ( 0 < args.length  ) {
                if ( args[0] == null ) 
                    return SchemeUtils.toSchemeString( "null" );
                else
                    return SchemeUtils.toSchemeString( args[0].getClass().getName() );
            } else {
                return SchemeUtils.NO_RESULT;
            }
        }
    }

    public static final TypeofBean typeofBean = new TypeofBean();
    public static final class TypeofBean extends PulsarProceduralDescriptiveDoc {
        {
            setCategory( Pulsar.DOCS_ID );
            setNames( "typeof" );
            setParameterDescription( "any" );
            addParameter( 0, "value", "any", null, false , "" );
            setReturnValueDescription( "::string" );
            setShortDescription( "||<name/>|| returns a Java class name of the specified value. " );
            setLongDescription( "In case the specified value is a ||null|| of Java, this procedure returns \"null\" as a string value. "
                                + "" 
                             );
        }
    }



    public static final LamuDocument isTrackBean=null;

    public static final IsTrackProc isTrackProc = new IsTrackProc(new String[] { "track?" });
    public static final class IsTrackProc extends MultipleNamedProcedure1 {
        public IsTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg0 ) throws Throwable {
            return arg0 instanceof MetroTrack;
        }
    }

    public static final LamuDocument trackToProcedureBean = null;

    public static final TrackToProcedureProc trackToProcedureProc = new TrackToProcedureProc(new String[] { "track->procedure" });
    public static final class TrackToProcedureProc extends MultipleNamedProcedure1 {
        public TrackToProcedureProc(String[] names) {
            super(names);
        }

        @Override
        public Object apply1(Object arg0 ) throws Throwable {
            if ( arg0 instanceof MetroTrack ) {
                return ((MetroTrack)arg0).getSequence();
            } else {
                throw new IllegalArgumentException( "the argument is not a track object." );
            }
        }
    }

    public static final LamuDocument applyTrackBean = null;
    
    public static final ApplyTrackProc applyTrackProc = new ApplyTrackProc(new String[] { "apply-track", "appt" });
    public static final class ApplyTrackProc extends MultipleNamedProcedureN {
        public ApplyTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            if ( args.length < 1 ) {
                throw new IllegalArgumentException("insufficient argument length (length<1)" );
            }
            Object arg0 = args[0];
            Object[] restArgs = Arrays.copyOfRange( args, 1, args.length );
            if ( arg0 instanceof Invokable ) {
                return ((Invokable)arg0).invoke( restArgs );
            } else {
                throw new IllegalArgumentException( "the argument is not an invokable object." );
            }
        }
    }

    public static final LamuDocument readTrackBean = null;

    public static final ReadTrackProc readTrackProc = new ReadTrackProc(new String[] { "read-track", "reat" });
    public static final class ReadTrackProc extends MultipleNamedProcedureN {
        public ReadTrackProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            if ( args.length < 1 ) {
                throw new IllegalArgumentException("insufficient argument length (length<1)" );
            }
            Object arg0 = args[0];
            if ( arg0 instanceof SchemeSequenceReadable ) {
                return ((SchemeSequenceReadable)arg0).readMusic();
            } else { 
                throw new IllegalArgumentException( "the argument is not a readable track. " + arg0 );
            }
        }
    }
    

    public static final RandomBean randomBean = new RandomBean();
    public static final class RandomBean extends PulsarProceduralDescriptiveDoc {
        {
            setCategory( Pulsar.DOCS_ID );
            setNames( "random", "rnd" );
            setParameterDescription( "[range::number]" );
            addParameter( 0, "range",     "number",  "1",  false , "" );
            setReturnValueDescription( "::number" );
            setShortDescription( "||<name/>|| generates a random number. " );
            setLongDescription( ""
                    + "This procedure adopts Mersenne Twister a random number generating algorithm. "
                    + "If an argument [range] is specified, the return value will be within 0<= x <[range]. "
                    + "If the argument is omitted, the range value defaults to 1. "
                    + "" 
            );
        }
    }
    public static final RandomProc randomProc = new RandomProc(new String[] { "random", "rnd" });
    public static final class RandomProc extends MultipleNamedProcedureN {
        private final MersenneTwisterFast random = new MersenneTwisterFast( new int[] { 
                (int) System.currentTimeMillis(),
                0x123, 0x234, 0x345, 0x456,
        });

        public RandomProc(String[] names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            synchronized ( random ) {
                switch ( args.length ) {
                case 0 :
                    return DFloNum.valueOf( random.nextDouble() );
                case 1 : {
                    double range = SchemeUtils.toDouble( args[0] );
                    return DFloNum.valueOf( random.nextDouble() * range );
                }
                default :
                {
                    double rangeMin = SchemeUtils.toDouble( args[0] );
                    double rangeMax = SchemeUtils.toDouble( args[1] );
                    double range    = rangeMax - rangeMin;

                    return DFloNum.valueOf( random.nextDouble() * range + rangeMin );
                }
                }
            }
        }
    }

    public static final LuckBean luckBean = new LuckBean();
    public static final class LuckBean extends PulsarProceduralDescriptiveDoc {
        {
            setCategory( Pulsar.DOCS_ID );
            setNames( "luck" );
            setParameterDescription( "[numeric]" );
            addParameter( 0, "probability",   "number",  "0.5",  false, "the probability to return #t." );
            setReturnValueDescription( "::boolean" );
            setShortDescription( "||<name/>|| is a procedure that returns a random boolean value. " );
            setLongDescription( "The first argument is the value of probability "
                    + "where the larger value causes the more probability of returning #t. "
                    + "When the specified value is equals or less than zero, the returning value is always #f. "
                    + "When the specified value is equals or larger than one the returning value is always #t. "
                    + "The only parameter can be omitted and in that case the default value one is applied. " );
        }
    }

    public static final LuckProc luckProc = new LuckProc( "luck" );
    public static final class LuckProc extends MultipleNamedProcedureN {
        private final MersenneTwisterFast random = new MersenneTwisterFast( new int[] { 
            (int) System.currentTimeMillis(),
            0x789, 0x89a, 0xabc, 0xbcd,
            (int) System.currentTimeMillis(),
    });
        public LuckProc(String ... names) {
            super(names);
        }

        @Override
        public Object applyN(Object[] args) throws Throwable {
            synchronized ( random ) {
                double probability = args.length == 0 ? 0.5 : SchemeUtils.toDouble( args[0] );
                if ( probability < 0 ) return false;
                if ( 1.0<=probability  ) return true;
                return random.nextBoolean( probability );
            }
        }
    }

    
    
    /**
     * Initializes an environment of scheme engine and defines API for the scripts.
     * 
     * @param scheme
     *            the scheme instance to initialize.
     */
    
    public static void initScheme( Environment env ) {
        SchemeUtils.defineLambda( env, getTrackPositionProc );
        SchemeUtils.defineLambda( env, printStackTraceProc);
        SchemeUtils.defineLambda( env, displayWarnProc);
        SchemeUtils.defineLambda( env, newlineWarnProc);
        SchemeUtils.defineLambda( env, typeofProc);
        SchemeUtils.defineLambda( env, isTrackProc);
        SchemeUtils.defineLambda( env, trackToProcedureProc);
        SchemeUtils.defineLambda( env, applyTrackProc);
        SchemeUtils.defineLambda( env, readTrackProc);
        SchemeUtils.defineLambda( env, randomProc);
        SchemeUtils.defineLambda( env, luckProc);
        PulsarLib_Notes.initScheme( env, PulsarNoteListParser.getInstance() );
    }
}
