/*
 * Trivia.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

import java.io.Serializable;

/**
 * Trivia is an interface defining behaviors to be shared by all implementing
 * subclasses.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public interface Trivia extends Serializable {
	
	/**
     * Determines if the provided string choice is the correct response for this trivia.
     * If the correct choice is supplied, this trivia is marked as answered correctly.
     * 
     * @return true if the correct choice is supplied, false otherwise
     */
	boolean isCorrect(final String theChoice);
	
	/**
     * Indicates whether or not this trivia has been answered correctly.
     * 
     * @return true if this trivia has been answered correctly, false otherwise
     */
	boolean isAnswered();
	
	/**
     * Provides this trivia's trivia type
     * 
     * @return the trivia type
     */
	TriviaType getTriviaType();
	
	/**
     * Provides the question for this trivia.
     * 
     * @return the question
     */
	String getQuestion();
	
	/**
     * Provides the correct value string for this trivia.
     * 
     * @return the correct value string
     */
	String getCorrectValue();
	
	/**
     * Provides the answer(s) for this trivia in a single string.
     * 
     * @return the answer string
     */
	String getAnswers();
	
	/**
     * Provides a copy of this trivia.
     * 
     * @return the copy of this trivia
     */
	Trivia copy();

}
