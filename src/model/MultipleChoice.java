package model;

import java.util.Arrays;

/**
 * Multiple choice trivia question.
 * 
 * @author Potafiy
 *
 */
public class MultipleChoice extends AbstractTrivia {
	
	private static final String ANSWER_PROMPT = "Please select an option below.\n";
	private final String[] myAnswers;

    public MultipleChoice(final String theCorrectLetter, final String[] theAnswers,
    													 final String theQuestion) {
        super(theCorrectLetter, theQuestion, ANSWER_PROMPT, TriviaType.MULTICHOICE);
        if (theCorrectLetter.length() != 1 || theCorrectLetter.charAt(0) < 'A' || 
        		                              theCorrectLetter.charAt(0) > 'D') {
        	throw new IllegalArgumentException("Invalid correct letter!");
        }
        myAnswers = Arrays.copyOf(theAnswers, theAnswers.length);
    }

	@Override
	public String getAnswers() {
		final StringBuilder sb = new StringBuilder();
		int index = 0;
		for (char option = 'A'; option <= 'D'; option++) {
			sb.append(String.format("%c. %s", myAnswers[index]));
			if (index < myAnswers.length - 1) {
				sb.append("\n");
			}
			index++;
		}
		return sb.toString();
	}
    
    
}