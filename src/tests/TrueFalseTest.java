/*
 * TrueFalseTest.java
 * TCSS 360 - Trivia Maze
 * Spring 2021
 */
package tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import model.TriviaType;
import test_utilities.TrueFalseForTests;

/**
 * TrueFalseTest is a test class used to test the {@link TrueFalse} class. Note,
 * however, that this test class uses {@link TrueFalseForTests} rather than
 * TrueFalse. This is done in order to maintain encapsulation. TrueFalse and
 * TrueFalseForTests contains the same code, thus, if you test one, you test the
 * other.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 1 June 2021
 *
 */
public class TrueFalseTest {
    /** A string used for the question input. */
    private static final String QUESTION = "a test question";

    /** A invalid string for a correct value. */
    private static final String INVALID = "not true or false";

    /** A null string to be used as invalid input. */
    private static final String NULL_STRING = null;

    /** Cheat code. */
    private static final String CHEAT = "beer";

    /** An empty string to be used as invalid input. */
    private static final String EMPTY = "";

    /** The answers of a true/false trivia */
    private static final String[] ANSWERS = { Boolean.TRUE.toString(), Boolean.FALSE.toString() };

    /** A trivia whose correct answer is 'true'. */
    private static TrueFalseForTests trueTrivia;
    /** A trivia whose correct answer is 'false'. */
    private static TrueFalseForTests falseTrivia;

    /**
     * Initializes new trivia.
     */
    @Before
    public void setup() {
        trueTrivia = new TrueFalseForTests(ANSWERS[0], QUESTION);
        falseTrivia = new TrueFalseForTests(ANSWERS[1], QUESTION);
    }

    /**
     * Test method for {@link TrueFalseForTest#getAnswers()}
     */
    @Test
    public void testGetAnswers() {
        assertEquals(ANSWERS[0] + "\n" + ANSWERS[1], trueTrivia.getAnswers());
    }

    /**
     * Test method for {@link TrueFalseForTest#getCorrectValue()} when the correct
     * value is 'true'.
     */
    @Test
    public void testGetCorrectValueTrue() {
        assertEquals(ANSWERS[0], trueTrivia.getCorrectValue());
    }

    /**
     * Test method for {@link TrueFalseForTest#getCorrectValue()} when the correct
     * value is 'false'.
     */
    @Test
    public void testGetCorrectValueFalse() {
        assertEquals(ANSWERS[1], falseTrivia.getCorrectValue());
    }

    /**
     * Test method for {@link TrueFalseForTest#getQuestion()}.
     */
    @Test
    public void testGetQuestion() {
        assertEquals(QUESTION, trueTrivia.getQuestion());
    }

    @Test
    public void testGetTriviaType() {
        assertEquals(TriviaType.TRUEFALSE, trueTrivia.getTriviaType());
    }

    /**
     * Test method for {@link TrueFalseForTest#getTriviaType()} when the answers is
     * correct.
     */
    @Test
    public void testIsCorrectWithCorrect() {
        assertTrue(trueTrivia.isCorrect(ANSWERS[0]));
    }

    /**
     * Test method for {@link TrueFalseForTest#isCorrect()} when the answers is
     * incorrect.
     */
    @Test
    public void testIsCorrectWithIncorrect() {
        assertFalse(trueTrivia.isCorrect(ANSWERS[1]));
    }

    /**
     * Test method for {@link TrueFalseForTest#isCorrect()} when the answers is the
     * cheat.
     */
    @Test
    public void testIsCorrectWithCheat() {
        assertTrue(trueTrivia.isCorrect(CHEAT));
    }

    /**
     * Test method for {@link TrueFalseForTest#isCorrect()} to check that an
     * exception is thrown when the input is null.
     */
    @Test
    public void testIsCorrectWithNull() {
        assertThrows(NullPointerException.class, () -> {
            trueTrivia.isCorrect(NULL_STRING);
        });
    }

    /**
     * Test method for {@link TrueFalseForTest#isAnswered()} before it is answered.
     */
    @Test
    public void testIsAnsweredNotAnswered() {
        assertFalse(trueTrivia.isAnswered());
    }

    /**
     * Test method for {@link TrueFalseForTest#isAnswered()} after it is answered.
     */
    @Test
    public void testIsAnsweredWhenAnswered() {
        trueTrivia.isCorrect(ANSWERS[0]);
        assertTrue(trueTrivia.isAnswered());
    }

    /**
     * Test method for {@link TrueFalseForTest#copy()} to check that the correct
     * value is copied.
     */
    @Test
    public void testCopyCorrect() {
        assertEquals(ANSWERS[0], trueTrivia.copy().getCorrectValue());
    }

    /**
     * Test method for {@link TrueFalseForTest#copy()} to check that the question is
     * copied.
     */
    @Test
    public void testCopyQuestion() {
        assertEquals(QUESTION, trueTrivia.copy().getQuestion());
    }

    /**
     * Test method for {@link TrueFalseForTest#TrueFalseForTests()} to check that an
     * exception is thrown when a null is input for the correct value.
     */
    @Test
    public void testNullCorrect() {
        assertThrows(NullPointerException.class, () -> {
            new TrueFalseForTests(NULL_STRING, QUESTION);
        });
    }

    /**
     * Test method for {@link TrueFalseForTest#TrueFalseForTests()} to check that an
     * exception is thrown when a null is input for the question.
     */
    @Test
    public void testNullQuestion() {
        assertThrows(NullPointerException.class, () -> {
            new TrueFalseForTests(ANSWERS[0], NULL_STRING);
        });
    }

    /**
     * Test method for {@link TrueFalseForTest#TrueFalseForTests()} to check that an
     * exception is thrown when an invalid input is used for the correct value.
     */
    @Test
    public void testInvalid() {
        assertThrows(IllegalArgumentException.class, () -> {
            new TrueFalseForTests(INVALID, QUESTION);
        });
    }

    /**
     * Test method for {@link TrueFalseForTest#TrueFalseForTests()} to check that an
     * exception is thrown when an empty String is used for the question.
     */
    @Test
    public void testEmptyQuestion() {
        assertThrows(IllegalArgumentException.class, () -> {
            new TrueFalseForTests(ANSWERS[0], EMPTY);
        });
    }

}
