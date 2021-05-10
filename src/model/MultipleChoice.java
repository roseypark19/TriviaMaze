package model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Multiple choice trivia question.
 * 
 * @author Potafiy
 *
 */
public class MultipleChoice extends AbstractTrivia {

    /**
     * These are the incorrect answers. Will have typical labels such as A,B,C,D.
     */
    final private List<String> myWrongChoices = new ArrayList<>();

    public MultipleChoice(final String theCorrectAnswer, final String thePrompt) {
        super(theCorrectAnswer, thePrompt);
    }

    public void addWrongChoice(final String theWrongChoice) {
        myWrongChoices.add(theWrongChoice);
    }

    public List<String> getWrongChoices() {
        return new ArrayList<String>(myWrongChoices);
    }
}