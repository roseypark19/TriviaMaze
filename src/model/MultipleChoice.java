/*
 * MultipleChoice.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

import java.util.Arrays;
import java.util.Objects;

/**
 * MultipleChoice is a class which implements behaviors specific to multiple choice trivia.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class MultipleChoice extends AbstractTrivia {

    /** The serial verison UID */
	private static final long serialVersionUID = 3234033564212707170L;
	
	/** The number of answer options for each multiple choice trivia */
	private static final int NUM_ANSWERS = 4;
	
	/** The answers for this MultipleChoice */
	private final String[] myAnswers;


    /**
     * Constructs a new MultipleChoice trivia with the provided correct letter value, array
     * of answers, and question.
     * 
     * @param theCorrectLetter the correct letter (A-D) for this multiple choice trivia
     * @param theAnswers the answers for this multiple choice trivia
     * @param theQuestion the question for this multiple choice trivia
     * @throws NullPointerException if theCorrectLetter, theAnswers, or theQuestion is null
     * @throws IllegalArgumentException if theCorrectLetter is not one character in length,
     *         if theCorrectLetter is not between A and D inclusive, the length of theAnswers
     *         is not equal to NUM_ANSWERS, if theQuestion is empty, or if any answers
     *         within theAnswers are empty
     */
    public MultipleChoice(final String theCorrectLetter, final String[] theAnswers, final String theQuestion) {
        super(theCorrectLetter, theQuestion, TriviaType.MULTICHOICE);
        Objects.requireNonNull(theCorrectLetter, "Correct letters must be non-null!");
        Objects.requireNonNull(theAnswers, "Answer arrays must be non-null!");
        Objects.requireNonNull(theQuestion, "Questions must be non-null!");
        if (theCorrectLetter.length() != 1 || theCorrectLetter.charAt(0) < 'A' || theCorrectLetter.charAt(0) > 'D') {
            throw new IllegalArgumentException("Invalid correct letter!");
        } else if (theAnswers.length != NUM_ANSWERS) {
        	throw new IllegalArgumentException("Invalid number of answers!");
        } else if (theQuestion.isEmpty()) {
        	throw new IllegalArgumentException("Questions must be at least one character!");
        }
        for (final String answer: theAnswers) {
        	if (answer.isEmpty()) {
        		throw new IllegalArgumentException("All answers must be at least one character!");
        	}
        }
        myAnswers = Arrays.copyOf(theAnswers, theAnswers.length);
    }

    @Override
    public String getAnswers() {
        final StringBuilder sb = new StringBuilder();
        int index = 0;
        for (char option = 'A'; option <= 'D'; option++) {
            sb.append(String.format("%c. %s", option, myAnswers[index]));
            if (index < myAnswers.length - 1) {
                sb.append("\n");
            }
            index++;
        }
        return sb.toString();
    }
    
    @Override
    public Trivia copy() {
    	return new MultipleChoice(getCorrectValue(), myAnswers, getQuestion());
    }

}