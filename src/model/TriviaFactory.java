package model;

public class TriviaFactory {

    /**
     * 
     * @param theCorrect  the correct string
     * @param theQuestion the question string
     * @param theType     the type of trivia enum
     * @param theAnswers  the array of answers [only needed when creating multiple
     *                    choice]
     * @return
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
