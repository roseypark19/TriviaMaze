package model;

import java.util.Objects;

/**
 * True or false trivia question.
 * 
 * @author Potafiy
 *
 */
public class TrueFalse extends AbstractTrivia {

	/**
	 * 
	 */
	private static final long serialVersionUID = -2292036415291204613L;
	private static final String[] ANSWERS = {Boolean.TRUE.toString(), 
			                                 Boolean.FALSE.toString()};

    public TrueFalse(final String theCorrectBool, final String theQuestion) {
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