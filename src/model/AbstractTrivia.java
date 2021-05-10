package model;

/**
 * Abstract Trivia question class.
 * 
 * @author Artem Potafiy
 *
 */
public abstract class AbstractTrivia {
    final private String myCorrectAnswer;
    /**
     * This is the prompt that is associated with the trivia question.
     * 
     */
    final private String myPrompt;

    public AbstractTrivia(final String theCorrectAnswer, final String thePrompt) {
        myCorrectAnswer = theCorrectAnswer.toLowerCase().trim();
        myPrompt = thePrompt.trim();
    }

    final public String getCorrectAnswer() {
        return myCorrectAnswer;
    }

    final public String getPrompt() {
        return myPrompt;
    }

    final public boolean isCorrect(final String thePlayerChoice) {
        return myCorrectAnswer.equals(thePlayerChoice.toLowerCase().trim());
    }
}