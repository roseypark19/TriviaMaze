package model;

/**
 * Short answer trivia question class.
 * 
 * @author Potafiy
 *
 */
public class ShortAnswer extends AbstractTrivia {
	
	private static final String ANSWER_PROMPT = "Please enter a response below.";

    public ShortAnswer(final String theCorrectResponse, final String theQuestion) {
        super(theCorrectResponse, theQuestion, ANSWER_PROMPT, QuestionType.SHORTANSWER);
        if (theCorrectResponse.equalsIgnoreCase(Boolean.TRUE.toString()) || 
        	theCorrectResponse.equalsIgnoreCase(Boolean.FALSE.toString())) {
        	throw new IllegalArgumentException("\"true\" and \"false\" not permitted!");
        }
    }

	@Override
	public String getAnswers() {
		return getCorrectValue();
	}

}