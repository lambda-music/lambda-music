{
	clientName : "Metro",
	outputPortNameList : [ "MIDI Output0", "MIDI Output1" ],
	optionalConnection : [ [ "Metro:MIDI Output1", "hydrogen-midi:RX" ] ,  ],
	process : (function(){
        var PEDAL_HH = 43;
        var RIDE = 73;
		var count = 100;
		return function( buf ){
			// buf.humanize( 0.0, 3 );
			// buf.noteShot( 0.0  , 1 , 0, 57, 105 );
			// buf.noteShot( 0.02 , 1 , 0, 74, 127 );
			// buf.noteShot( 0.2  , 1 , 0, RIDE, 100 );
			// buf.noteShot( 0.4  , 1 , 0, RIDE, 100 );
			// buf.noteShot( 0.6  , 1 , 0, RIDE, 100 );
			// buf.noteShot( 0.8  , 1 , 0, RIDE, 100 );
			// buf.length(     1.00 );

			buf.humanize( 0.0, 0 );

			buf.noteShot( ( 0 ) / 4 - 0.0,  1 , 0, RIDE, 70 );
			buf.noteShot( ( 1 ) / 4 - 0.0,  1 , 0, RIDE, 96 );
			buf.noteShot( ( 2 ) / 4 - 0.0,  1 , 0, RIDE, 70 );
			buf.noteShot( ( 3 ) / 4 - 0.0 , 1 , 0, RIDE, 96 );

			// buf.noteShot( ( 1 + 0.5 ) / 4 - 0.1,  1 , 0, RIDE, 120 );
			// buf.noteShot( ( 3 + 0.5 ) / 4 - 0.1 , 1 , 0, RIDE, 126 );

			buf.noteShot( ( 1 + 0.0 ) / 4 - 0.0,  1 , 0, PEDAL_HH,  96 );
			buf.noteShot( ( 3 + 0.0 ) / 4 - 0.0 , 1 , 0, PEDAL_HH, 106 );

			buf.length(     1.00 );
			
			// metro.spawn( 0.1, 
			// 	(function() {
			// 		var cnt = 2;
			// 		return function( buf ) {
			// 			//				buf.noteShot( 0.5d  , 1 , 0, 57, 127 );
	
			// 			buf.noteShot( 0.0  , 1 , 0, 63, 127 );
			// 			buf.noteShot( 0.2  , 1 , 0, 63, 80 );
			// 			buf.noteShot( 0.4  , 1 , 0, 63, 80 );
			// 			buf.noteShot( 0.6  , 1 , 0, 63, 80 );
			// 			buf.noteShot( 0.8  , 1 , 0, 63, 80 );
			// 			buf.length(1.0);
			// 			return 0<cnt--;
			// 		};
			// 	})()
			// );
			return 0 < count--; 
		
		};
	})(),
}
