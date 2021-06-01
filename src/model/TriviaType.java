/*
 * TriviaType.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

/**
 * TriviaType is an enum to represent different types of trivia in the Maze Hops
 * game. The string for each trivia type corresponds to an abbreviation of it's 
 * full name (i.e multiple choice -> "MC").
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public enum TriviaType {
	
	/** A multiple choice trivia */
	MULTICHOICE("MC"),
	
	/** A true/false trivia */
	TRUEFALSE("TF"),
	
	/** A short answer trivia */
	SHORTANSWER("SA");
	
	/** The string type assigned to this trivia type */
	private final String myType;
	
	/**
	 * Constructs a new TriviaType with the specified string type.
	 * 
	 * @param theType the string type to be assigned
	 */
	TriviaType(final String theType) {
		myType = theType;
	}

}
