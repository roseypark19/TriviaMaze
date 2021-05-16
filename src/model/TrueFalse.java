package model;

/**
 * True or false trivia question.
 * 
 * @author Potafiy
 *
 */
public class TrueFalse extends AbstractTrivia {

	private static final String[] ANSWERS = {Boolean.TRUE.toString(), 
			                                 Boolean.FALSE.toString()};
	private static final String ANSWER_PROMPT = "Please select true or false below.";

    public TrueFalse(final String theCorrectBool, final String theQuestion) {
        super(theCorrectBool, theQuestion, ANSWER_PROMPT, TriviaType.TRUEFALSE);
        if (!theCorrectBool.equalsIgnoreCase(ANSWERS[0]) && 
        	!theCorrectBool.equalsIgnoreCase(ANSWERS[1])) {
        	throw new IllegalArgumentException("Invalid correct answer!");
        }
    }

	@Override
	public String getAnswers() {
		return ANSWERS[0] + "\n" + ANSWERS[1];
	}

}