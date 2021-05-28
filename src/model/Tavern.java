package model;

import java.io.Serializable;

public class Tavern implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2779242966777052959L;
	private final Trivia myTrivia;
	
	public Tavern(final Trivia theTrivia) {
		myTrivia = theTrivia;
	}
	
	public Trivia getTrivia() {
		return myTrivia;
	}
}
