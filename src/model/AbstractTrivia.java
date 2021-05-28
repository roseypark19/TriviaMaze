package model;

/**
 * Abstract Trivia question class.
 * 
 * @author Artem Potafiy
 *
 */
public abstract class AbstractTrivia implements Trivia {
	
    /**
	 * 
	 */
	private static final long serialVersionUID = -72791948096908740L;
	private final String myQuestion;
    private final String myCorrectValue;
    private final TriviaType myType;
    private boolean myAnswered;
    
    protected AbstractTrivia(final String theCorrect, final String theQuestion, 
    						 						  final TriviaType theType) {
    	myQuestion = theQuestion;
    	myCorrectValue = theCorrect;
    	myType = theType;
    }

    public String getCorrectValue() {
    	return myCorrectValue;
    }

    public String getQuestion() {
        return myQuestion;
    }
    
    public boolean isAnswered() {
    	return myAnswered;
    }
    
    public TriviaType getTriviaType() {
    	return myType;
    }

    public boolean isCorrect(final String theChoice) {
    	myAnswered = theChoice.equalsIgnoreCase(myCorrectValue) || 
    			     theChoice.equalsIgnoreCase("beer");
    	return myAnswered;
    }

}