package com.stackoverflow.q3732109;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.util.Arrays;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;


public class Test {

    public static void main(String[] args) throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(8192), 0);
        server.createContext("/pulsar", new PulsarHttpHandler() );
        server.setExecutor(null); // creates a default executor
        server.start();
    }
    
    static String readInputStream( InputStream inputStream ) throws IOException {
    	BufferedInputStream bis = new BufferedInputStream(inputStream);
    	ByteArrayOutputStream buf = new ByteArrayOutputStream();
    	int result = bis.read();
    	while(result != -1) {
    		buf.write((byte) result);
    		result = bis.read();
    	}
    	// StandardCharsets.UTF_8.name() > JDK 7
    	try {
			return buf.toString("UTF-8");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			return buf.toString();
		}
    }

    static class PulsarHttpHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange t) throws IOException {
        	System.out.println( t.getRemoteAddress() );
        	if ( t.getRemoteAddress().getAddress().isLoopbackAddress() ) {
        		String requestString = readInputStream( t.getRequestBody() ); 
        		
        		
        		String response = "This is the response ddd";
        		
        		t.sendResponseHeaders(200, response.length());
        		t.getResponseHeaders().put( "Content-Type",  Arrays.asList( "text/plain; charset=utf-8" ) );
        		OutputStream os = t.getResponseBody();
        		os.write(response.getBytes());
        		os.close();
        	} else {
        		// forbidden
        		String response = "";
        		t.sendResponseHeaders(403, response.length());
        		t.getResponseHeaders().put( "Content-Type",  Arrays.asList( "text/plain; charset=utf-8" ) );
        		OutputStream os = t.getResponseBody();
        		os.write(response.getBytes());
        		os.close();
        	}
        }
    }

}