package model;

import java.util.Objects;

/**
 * Short answer trivia question class.
 * 
 * @author Potafiy
 *
 */
public class ShortAnswer extends AbstractTrivia {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1993985826483549999L;

	public ShortAnswer(final String theCorrectResponse, final String theQuestion) {
        super(theCorrectResponse, theQuestion, TriviaType.SHORTANSWER);
        Objects.requireNonNull(theCorrectResponse, "Correct responses must be non-null!");
        Objects.requireNonNull(theQuestion, "Questions must be non-null!");
        if (theCorrectResponse.equalsIgnoreCase(Boolean.TRUE.toString()) || 
        	theCorrectResponse.equalsIgnoreCase(Boolean.FALSE.toString())) {
        	throw new IllegalArgumentException("\"true\" and \"false\" not permitted!");
        } else if (theCorrectResponse.isEmpty() || theQuestion.isEmpty()) {
        	throw new IllegalArgumentException("Correct responses and questions must be at least one character!");
        }
    }

	@Override
	public String getAnswers() {
		return getCorrectValue();
	}
	
	@Override
	public Trivia copy() {
		return new ShortAnswer(getCorrectValue(), getQuestion());
	}

}