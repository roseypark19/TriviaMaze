/*
 * AbstractTrivia.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

import java.util.Objects;

/**
 * AbstractTrivia is a class that implements attributes and behaviors commonly shared
 * by implementing subclasses.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public abstract class AbstractTrivia implements Trivia {
	
    /** The serial version UID */
	private static final long serialVersionUID = -72791948096908740L;
	
	/** The cheat keyword for short answer responses */
	private static final String CHEAT = "beer";
	
	/** The trivia question */
	private final String myQuestion;
	
	/** The trivia correct value */
    private final String myCorrectValue;
    
    /** The trivia type */
    private final TriviaType myType;
    
    /** A true/false indication of whether or not this trivia has been answered */
    private boolean myAnswered;
    
    /**
     * Constructs a new AbstractTrivia using the provided correct response, question, and
     * trivia type.
     * 
     * @param theCorrect the correct response string
     * @param theQuestion the question
     * @param theType the trivia type
     */
    protected AbstractTrivia(final String theCorrect, final String theQuestion, 
    						 						  final TriviaType theType) {
    	myQuestion = theQuestion;
    	myCorrectValue = theCorrect;
    	myType = theType;
    }

    @Override
    public String getCorrectValue() {
    	return myCorrectValue;
    }

    @Override
    public String getQuestion() {
        return myQuestion;
    }
    
    @Override
    public boolean isAnswered() {
    	return myAnswered;
    }
    
    @Override
    public TriviaType getTriviaType() {
    	return myType;
    }

    @Override
    public boolean isCorrect(final String theChoice) {
    	Objects.requireNonNull(theChoice, "Answer choices must be non-null!");
    	myAnswered = theChoice.equalsIgnoreCase(myCorrectValue) || 
    			     theChoice.equalsIgnoreCase(CHEAT);
    	return myAnswered;
    }

}