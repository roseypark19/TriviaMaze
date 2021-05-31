package model;

import java.io.Serializable;
import java.util.Objects;

public class Tavern implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2779242966777052959L;
	private final Trivia myTrivia;
	
	public Tavern(final Trivia theTrivia) {
		Objects.requireNonNull(theTrivia, "Trivias must be non-null!");
		myTrivia = theTrivia;
	}
	
	public Trivia getTrivia() {
		return myTrivia;
	}
}
