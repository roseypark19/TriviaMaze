/*
 * MultipleChoiceTest.java
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
import test_utilities.MultipleChoiceForTests;

/**
 * MultipleChoiceTest is a test class used to test the {@link MultipleChoice}
 * class. Note, however, that this test class uses
 * {@link MultipleChoiceForTests} rather than MultipleChoice. This is done in
 * order to maintain encapsulation. MultipleChoice and MultipleChoiceForTests
 * contains the same code, thus, if you test one, you test the other.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 1 June 2021
 *
 */
public class MultipleChoiceTest {

    /** A String used for the correct letter. */
    private static final String CORRECT = "A";

    /** A string used for the question input. */
    private static final String QUESTION = "a test question";

    /** A invalid string for a correct value. */
    private static final String INVALID = "an invalid answer";

    /** A null String used for invalid input. */
    private static final String NULL_STRING = null;

    /** A null String array used for invalid input. */
    private static final String[] NULL_STRING_ARR = null;

    /** Cheat code. */
    private static final String CHEAT = "beer";

    /** An empty String used for invalid input. */
    private static final String EMPTY = "";

    /** Valid array of answers. */
    private static final String[] ANSWERS = { "test", "test", "test", "test" };

    /** Invalid array of answers which is the incorrect size. */
    private static final String[] INVALID_SIZE_ANSWERS = { "test" };

    /** Ivalid array of answers which contains empty Strings. */
    private static final String[] INVALID_EMPTY_ANSWERS = { "", "", "", "" };

    /** Answers formated. */
    private static final String ANSWERS_FORMAT = "A. test\nB. test\nC. test\nD. test";

    /** Trivia question. */
    private static MultipleChoiceForTests trivia;

    /**
     * Initializes new trivia.
     */
    @Before
    public void setup() {
        trivia = new MultipleChoiceForTests(CORRECT, ANSWERS, QUESTION);
    }

    /**
     * Test method for {@link MultipleChoiceForTest#getAnswers()}
     */
    @Test
    public void testGetAnswers() {
        assertEquals(ANSWERS_FORMAT, trivia.getAnswers());
    }

    /**
     * Test method for {@link MultipleChoiceForTest#getCorrectValue()}
     */
    @Test
    public void testGetCorrectValue() {
        assertEquals(CORRECT, trivia.getCorrectValue());
    }

    /**
     * Test method for {@link MultipleChoiceForTest#getQuestion()}
     */
    @Test
    public void testGetQuestion() {
        assertEquals(QUESTION, trivia.getQuestion());
    }

    /**
     * Test method for {@link MultipleChoiceForTest#getTriviaType()}
     */
    @Test
    public void testGetTriviaType() {
        assertEquals(TriviaType.MULTICHOICE, trivia.getTriviaType());
    }

    /**
     * Test method for {@link MultipleChoiceForTest#isCorrect()} when using the
     * correct value.
     */
    @Test
    public void testIsCorrectWithCorrect() {
        assertTrue(trivia.isCorrect(CORRECT));
    }

    /**
     * Test method for {@link MultipleChoiceForTest#isCorrect()} when using an
     * incorrect value.
     */
    @Test
    public void testIsCorrectWithIncorrect() {
        assertFalse(trivia.isCorrect(INVALID));
    }

    /**
     * Test method for {@link MultipleChoiceForTest#isCorrect()} when using the
     * cheat value.
     */
    @Test
    public void testIsCorrectWithCheat() {
        assertTrue(trivia.isCorrect(CHEAT));
    }

    /**
     * Test method for {@link MultipleChoiceForTest#isCorrect()} when using a null
     * value.
     */
    @Test
    public void testIsCorrectWithNull() {
        assertThrows(NullPointerException.class, () -> {
            trivia.isCorrect(NULL_STRING);
        });
    }

    /**
     * Test method for {@link MultipleChoiceForTest#isAnswered()} before the trivia
     * is answered.
     */
    @Test
    public void testIsAnsweredNotAnswered() {
        assertFalse(trivia.isAnswered());
    }

    /**
     * Test method for {@link MultipleChoiceForTest#isAnswered()} after the trivia
     * is answered.
     */
    @Test
    public void testIsAnsweredWhenAnswered() {
        trivia.isCorrect(CORRECT);
        assertTrue(trivia.isAnswered());
    }

    /**
     * Test method for {@link MultipleChoideForTest#copy()} to check that the
     * correct value is copied.
     */
    @Test
    public void testCopyCorrect() {
        assertEquals(CORRECT, trivia.copy().getCorrectValue());
    }

    /**
     * Test method for {@link MultipleChoideForTest#copy()} to check that the
     * question is copied.
     */
    @Test
    public void testCopyQuestion() {
        assertEquals(QUESTION, trivia.copy().getQuestion());
    }

    /**
     * Test method for {@link MultipleChoideForTest#copy()} to check that the
     * answers are copied.
     */
    @Test
    public void testCopyAnswers() {
        assertEquals(ANSWERS_FORMAT, trivia.copy().getAnswers());
    }

    /**
     * Test method for {@link MultipleChoideForTest#MultipleChoiceForTests()} to
     * check if an exception is thrown when a null is input for the correct value.
     */
    @Test
    public void testNullCorrect() {
        assertThrows(NullPointerException.class, () -> {
            new MultipleChoiceForTests(NULL_STRING, ANSWERS, QUESTION);
        });
    }

    /**
     * Test method for {@link MultipleChoideForTest#MultipleChoiceForTests()} to
     * check if an exception is thrown when a null is input for the answers.
     */
    @Test
    public void testNullAnswers() {
        assertThrows(NullPointerException.class, () -> {
            new MultipleChoiceForTests(CORRECT, NULL_STRING_ARR, QUESTION);
        });
    }

    /**
     * Test method for {@link MultipleChoideForTest#MultipleChoiceForTests()} to
     * check if an exception is thrown when a null is input for the question.
     */
    @Test
    public void testNullQuestion() {
        assertThrows(NullPointerException.class, () -> {
            new MultipleChoiceForTests(CORRECT, ANSWERS, NULL_STRING);
        });
    }

    /**
     * Test method for {@link MultipleChoideForTest#MultipleChoiceForTests()} to
     * check if an exception is thrown when the correct value is the wrong size.
     */
    @Test
    public void testInvalidSizeCorrect() {
        assertThrows(IllegalArgumentException.class, () -> {
            new MultipleChoiceForTests(INVALID, ANSWERS, QUESTION);
        });
    }

    /**
     * Test method for {@link MultipleChoideForTest#MultipleChoiceForTests()} to
     * check if an exception is thrown when the correct value is the proper size but
     * the ascii value is higher than 'D'.
     */
    @Test
    public void testInvalidLetterHiAscii() {
        final String invalidLetter = "E"; // ascii D<E
        assertThrows(IllegalArgumentException.class, () -> {
            new MultipleChoiceForTests(invalidLetter, ANSWERS, QUESTION);
        });
    }

    /**
     * Test method for {@link MultipleChoideForTest#MultipleChoiceForTests()} to
     * check if an exception is thrown when the correct value is the proper size but
     * the ascii value is lower than 'A'.
     */
    @Test
    public void testInvalidOtherLetterLoAscii() {
        final String invalidLetter = "@"; // ascii @<A
        assertThrows(IllegalArgumentException.class, () -> {
            new MultipleChoiceForTests(invalidLetter, ANSWERS, QUESTION);
        });
    }

    /**
     * Test method for {@link MultipleChoideForTest#MultipleChoiceForTests()} to
     * check if an exception is thrown when the question is an empty String.
     */
    @Test
    public void testEmptyQuestion() {
        assertThrows(IllegalArgumentException.class, () -> {
            new MultipleChoiceForTests(CORRECT, ANSWERS, EMPTY);
        });
    }

    /**
     * Test method for {@link MultipleChoideForTest#MultipleChoiceForTests()} to
     * check if an exception is thrown when the array of answers is not the proper
     * size.
     */
    @Test
    public void testInvalidSizeAnswers() {
        assertThrows(IllegalArgumentException.class, () -> {
            new MultipleChoiceForTests(CORRECT, INVALID_SIZE_ANSWERS, QUESTION);
        });
    }

    /**
     * Test method for {@link MultipleChoideForTest#MultipleChoiceForTests()} to
     * check if an exception is thrown when the array of answers contains and empty
     * String.
     */
    @Test
    public void testInvalidEmptyAnswers() {
        assertThrows(IllegalArgumentException.class, () -> {
            new MultipleChoiceForTests(CORRECT, INVALID_EMPTY_ANSWERS, QUESTION);
        });
    }
}
