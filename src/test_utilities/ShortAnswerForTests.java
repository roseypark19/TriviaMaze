/*
 * ShortAnswerForTests.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package test_utilities;

import model.AbstractTrivia;
import model.Trivia;
import model.TriviaType;

import java.util.Objects;

/**
 * {@link ShortAnswerForTests} is the exact same as the {@link ShortAnswer}
 * class. The only difference is that the constructor in this class is public.
 * This is done to maintain encapsulation in {@link ShortAnswer} while testing
 * said class.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class ShortAnswerForTests extends AbstractTrivia {

    /** The serial version UID */
    private static final long serialVersionUID = 1993985826483549999L;

    /**
     * Constructs a new ShortAnswer trivia with the provided correct response and
     * question.
     * 
     * @param theCorrectResponse the correct short answer response for this short
     *                           answer trivia
     * @param theQuestion        the question for this short answer trivia
     * @throws NullPointerException     if theCorrectResponse or theQuestion is null
     * @throws IllegalArgumentException if "true" or "false" is supplied as a
     *                                  correct response or theCorrectResponse is
     *                                  empty or theQuestion is empty
     */
    public ShortAnswerForTests(final String theCorrectResponse, final String theQuestion) {
        super(theCorrectResponse, theQuestion, TriviaType.SHORTANSWER);
        Objects.requireNonNull(theCorrectResponse, "Correct responses must be non-null!");
        Objects.requireNonNull(theQuestion, "Questions must be non-null!");
        if (theCorrectResponse.equalsIgnoreCase(Boolean.TRUE.toString())
                || theCorrectResponse.equalsIgnoreCase(Boolean.FALSE.toString())) {
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
        return new ShortAnswerForTests(getCorrectValue(), getQuestion());
    }

}