/*
 * TriviaFactory.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

/**
 * TriviaFactory is a class for creating trivia objects for the Maze Hops game. This
 * factory creates trivia according to the supplied trivia type.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class TriviaFactory {
 
    /**
     * Creates and supplies a new trivia object using the provided trivia attributes
     * and trivia type.
     * 
     * @param theCorrect the correct choice string 
     * @param theQuestion the trivia question
     * @param theType the trivia type
     * @param theAnswers the answers for this trivia
     * @return the new trivia object
     */
    public static Trivia createTrivia(final String theCorrect, 
    		                          final String theQuestion, 
    		                          final TriviaType theType,
                                      final String[] theAnswers) {   	
        Trivia trivia = null;
        switch (theType) {
        case SHORTANSWER:
            trivia = new ShortAnswer(theCorrect, theQuestion);
            break;
        case MULTICHOICE:
            trivia = new MultipleChoice(theCorrect, theAnswers, theQuestion);
            break;
        case TRUEFALSE:
            trivia = new TrueFalse(theCorrect, theQuestion);
            break;
        }
        return trivia;
    }

}
