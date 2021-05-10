package model;

/**
 * True or false trivia question.
 * 
 * @author Potafiy
 *
 */
public class TrueFalse extends MultipleChoice {

    final private String TRUE = "true";
    final private String FALSE = "false";

    public TrueFalse(String theCorrectAnswer, String thePrompt) {
        super(theCorrectAnswer, thePrompt);
        if (this.getCorrectAnswer().equals(TRUE) || this.getCorrectAnswer().equals(FALSE)) {
            throw new IllegalArgumentException();
        }
    }

    public boolean getBooleanAnswer() {
        if (this.getCorrectAnswer().equals(TRUE)) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void addWrongChoice(final String theWrongChoice) {
        if (this.getWrongChoices().size() != 0) { // there can only be one wrong answer
            throw new IllegalArgumentException("No more wrong choices can be added to TrueFalse trivia.");
        }
        this.addWrongChoice(theWrongChoice);
    }
}