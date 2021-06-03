/*
 * Tavern.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

import java.io.Serializable;
import java.util.Objects;

/**
 * Tavern is a class for storing and providing access to the trivia questions presented
 * in the Maze Hops game.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class Tavern implements Serializable {
	
	/** The serial version UID */
	private static final long serialVersionUID = 2779242966777052959L;
	
	/** This tavern's trivia */
	private final Trivia myTrivia;
	
	/**
	 * Constructs a new Tavern with the provided trivia.
	 * 
	 * @param theTrivia the trivia to be assigned to this tavern
	 * @throws NullPointerException if theTrivia is null
	 */
	Tavern(final Trivia theTrivia) {
		Objects.requireNonNull(theTrivia, "Trivias must be non-null!");
		myTrivia = theTrivia;
	}
	
	/**
	 * Provides this tavern's trivia
	 * 
	 * @return the trivia
	 */
	Trivia getTrivia() {
		return myTrivia;
	}
}
