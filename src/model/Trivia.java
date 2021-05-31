package model;

import java.io.Serializable;

public interface Trivia extends Serializable {
	
	boolean isCorrect(final String theAnswerKey);
	
	boolean isAnswered();
	
	TriviaType getTriviaType();
	
	String getQuestion();
	
	String getCorrectValue();
	
	String getAnswers();
	
	Trivia copy();

}
