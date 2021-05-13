package model;

/**
 * Abstract Trivia question class.
 * 
 * @author Artem Potafiy
 *
 */
public abstract class AbstractTrivia implements Trivia {
	
    private final String myQuestion;
    private final String myAnswerPrompt;
    private final String myCorrectValue;
    private final QuestionType myType;
    private boolean myAnswered;
    
    protected AbstractTrivia(final String theCorrect, final String theQuestion, 
    						 final String theAnswerPrompt, final QuestionType theType) {
    	myQuestion = theQuestion;
    	myAnswerPrompt = theAnswerPrompt;
    	myCorrectValue = theCorrect;
    	myType = theType;
    }

    public String getCorrectValue() {
    	return myCorrectValue;
    }

    public String getQuestion() {
        return myQuestion;
    }
    
    public String getAnswerPrompt() {
    	return myAnswerPrompt;
    }
    
    public boolean isAnswered() {
    	return myAnswered;
    }
    
    public QuestionType getTriviaType() {
    	return myType;
    }

    public boolean isCorrect(final String theChoice) {
    	myAnswered = theChoice.equalsIgnoreCase(myCorrectValue);
    	return myAnswered;
    }
}