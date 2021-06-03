/*
 * ShortAnswerTest.java
 * TCSS 360 - Trivia Maze
 * Spring 2021
 */
package tests;

import static org.junit.Assert.*;

import org.junit.BeforeClass;
import org.junit.Test;

import model.TriviaType;
import test_utilities.ShortAnswerForTests;

/**
 * ShortAnswerTest is a test class used to test the {@link ShortAnswer} class.
 * Note, however, that this test class uses {@linkShortAnswerForTests} rather
 * than ShortAnswer. This is done in order to maintain encapsulation.
 * ShortAnswer and ShortAnswerForTests contains the same code, thus, if you test
 * one, you test the other.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 1 June 2021
 *
 */
public class ShortAnswerTest {

    /** A string used for the question input. */
    private static final String CORRECT = "a test correct value";

    /** A string used for the question input. */
    private static final String QUESTION = "a test question";

    /** Used as incorrect answer. */
    private static final String INCORRECT = "an incorrect answer";

    /** A null String used for invalid input. */
    private static final String NULL_STRING = null;

    /** Cheat code. */
    private static final String CHEAT = "beer";

    /** An empty String used for invalid input. */
    private static final String EMPTY = "";

    /** A trivia question. */
    private static ShortAnswerForTests trivia;

    /**
     * Initializes new trivia.
     */
    @BeforeClass
    public static void setup() {
        trivia = new ShortAnswerForTests(CORRECT, QUESTION);
    }

    /**
     * Test method for {@link ShortAnswerForTest#getAnswers()}
     */
    @Test
    public void testGetAnswers() {
        assertEquals(CORRECT, trivia.getAnswers());
    }

    /**
     * Test method for {@link ShortAnswerForTest#getCorrectValue()}.
     */
    @Test
    public void testGetCorrectValue() {
        assertEquals(CORRECT, trivia.getCorrectValue());
    }

    /**
     * Test method for {@link ShortAnswerForTest#getQuestion()}.
     */
    @Test
    public void testGetQuestion() {
        assertEquals(QUESTION, trivia.getQuestion());
    }

    /**
     * Test method for {@link ShortAnswerForTest#getTriviaType()}.
     */
    @Test
    public void testGetTriviaType() {
        assertEquals(TriviaType.SHORTANSWER, trivia.getTriviaType());
    }

    /**
     * Test method for {@link ShortAnswerForTest#isCorrect()} when the correct value
     * is used.
     */
    @Test
    public void testIsCorrectWithCorrect() {
        assertTrue(trivia.isCorrect(CORRECT));
    }

    /**
     * Test method for {@link ShortAnswerForTest#isCorrect()} when an incorrect
     * value is used.
     */
    @Test
    public void testIsCorrectWithIncorrect() {
        assertFalse(trivia.isCorrect(INCORRECT));
    }

    /**
     * Test method for {@link ShortAnswerForTest#isCorrect()} when the cheat value
     * is used.
     */
    @Test
    public void testIsCorrectWithCheat() {
        assertTrue(trivia.isCorrect(CHEAT));
    }

    /**
     * Test method for {@link ShortAnswerForTest#isCorrect()} when a null value is
     * used.
     */
    @Test
    public void testIsCorrectWithNull() {
        assertThrows(NullPointerException.class, () -> {
            trivia.isCorrect(NULL_STRING);
        });
    }

    /**
     * Test method for {@link ShortAnswerForTest#isAnswered()} before the trivia is
     * answered.
     */
    @Test
    public void testIsAnsweredNotAnswered() {
        assertFalse(trivia.isAnswered());
    }

    /**
     * Test method for {@link ShortAnswerForTest#isAnswered()} after the trivia is
     * answered.
     */
    @Test
    public void testIsAnsweredWhenAnswered() {
        trivia.isCorrect(CORRECT);
        assertTrue(trivia.isAnswered());
    }

    /**
     * Test method for {@link ShortAnswerForTest#copy()} to check that the correct
     * value is copied.
     */
    @Test
    public void testCopyCorrect() {
        assertEquals(CORRECT, trivia.copy().getCorrectValue());
    }

    /**
     * Test method for {@link ShortAnswerForTest#copy()} to check that the correct
     * question is copied.
     */
    @Test
    public void testCopyQuestion() {
        assertEquals(QUESTION, trivia.copy().getQuestion());
    }

    /**
     * Test method for {@link ShortAnswerForTest#ShortAnswerForTests()} to check
     * that an exception is thrown for a null input for the correct value.
     */
    @Test
    public void testNullCorrect() {
        assertThrows(NullPointerException.class, () -> {
            new ShortAnswerForTests(NULL_STRING, QUESTION);
        });
    }

    /**
     * Test method for {@link ShortAnswerForTest#ShortAnswerForTests()} to check
     * that an exception is thrown for a null input for the question.
     */
    @Test
    public void testNullQuestion() {
        assertThrows(NullPointerException.class, () -> {
            new ShortAnswerForTests(CORRECT, NULL_STRING);
        });
    }

    /**
     * Test method for {@link ShortAnswerForTest#ShortAnswerForTests()} to check
     * that an exception is thrown when 'true' is input for the correct value.
     */
    @Test
    public void testTrueCorrect() {
        assertThrows(IllegalArgumentException.class, () -> {
            new ShortAnswerForTests(Boolean.TRUE.toString(), QUESTION);
        });
    }

    /**
     * Test method for {@link ShortAnswerForTest#ShortAnswerForTests()} to check
     * that an exception is thrown when 'false' is input for the correct value.
     */
    @Test
    public void testFalseCorrect() {
        assertThrows(IllegalArgumentException.class, () -> {
            new ShortAnswerForTests(Boolean.FALSE.toString(), QUESTION);
        });
    }

    /**
     * Test method for {@link ShortAnswerForTest#ShortAnswerForTests()} to check
     * that an exception is thrown when an empty String is input for the correct
     * value.
     */
    @Test
    public void testEmptyCorrect() {
        assertThrows(IllegalArgumentException.class, () -> {
            new ShortAnswerForTests(EMPTY, QUESTION);
        });
    }

    /**
     * Test method for {@link ShortAnswerForTest#ShortAnswerForTests()} to check
     * that an exception is thrown when an empty String is input for the question.
     */
    @Test
    public void testEmptyQuestion() {
        assertThrows(IllegalArgumentException.class, () -> {
            new ShortAnswerForTests(CORRECT, EMPTY);
        });
    }
}
