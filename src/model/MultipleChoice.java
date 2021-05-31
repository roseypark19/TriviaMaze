package model;

import java.util.Arrays;
import java.util.Objects;

/**
 * Multiple choice trivia question.
 * 
 * @author Potafiy
 *
 */
public class MultipleChoice extends AbstractTrivia {

    /**
	 * 
	 */
	private static final long serialVersionUID = 3234033564212707170L;
	private static final int NUM_ANSWERS = 4;
	private final String[] myAnswers;

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