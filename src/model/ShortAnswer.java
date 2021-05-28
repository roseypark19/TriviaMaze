package model;

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
        if (theCorrectResponse.equalsIgnoreCase(Boolean.TRUE.toString()) || 
        	theCorrectResponse.equalsIgnoreCase(Boolean.FALSE.toString())) {
        	throw new IllegalArgumentException("\"true\" and \"false\" not permitted!");
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