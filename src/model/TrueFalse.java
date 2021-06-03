/*
 * TrueFalse.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

import java.util.Objects;

/**
 * TrueFalse is a class which implements behaviors specific to true/false trivia.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class TrueFalse extends AbstractTrivia {

	/** The serial version UID */
	private static final long serialVersionUID = -2292036415291204613L;
	
	/** The answers of a true/false trivia */
	private static final String[] ANSWERS = {Boolean.TRUE.toString(), 
			                                 Boolean.FALSE.toString()};


	/**
     * Constructs a new TrueFalse trivia with the provided correct boolean string response 
     * and question.
     * 
     * @param theCorrectBool the correct boolean string response for this true/false trivia
     * @param theQuestion the question for this true/false trivia
     * @throws NullPointerException if theCorrectBool or theQuestion is null
     * @throws IllegalArgumentException if "true" or "false" is not supplied as a correct 
     *         response or theQuestion is empty
     */
    TrueFalse(final String theCorrectBool, final String theQuestion) {
        super(theCorrectBool, theQuestion, TriviaType.TRUEFALSE);
        Objects.requireNonNull(theCorrectBool, "Correct boolean strings must be non-null!");
        Objects.requireNonNull(theQuestion, "Questions must be non-null!");
        if (!theCorrectBool.equalsIgnoreCase(ANSWERS[0]) && 
        	!theCorrectBool.equalsIgnoreCase(ANSWERS[1])) {
        	throw new IllegalArgumentException("Invalid correct answer! Answers must be true/false.");
        } else if (theQuestion.isEmpty()) {
        	throw new IllegalArgumentException("Questions must be at least one character!");
        }
    }

	@Override
	public String getAnswers() {
		return ANSWERS[0] + "\n" + ANSWERS[1];
	}
	
	@Override
	public Trivia copy() {
		return new TrueFalse(getCorrectValue(), getQuestion());
	}

}