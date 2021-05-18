package model;

public interface Trivia {
	
	boolean isCorrect(final String theAnswerKey);
	
	boolean isAnswered();
	
	TriviaType getTriviaType();
	
	String getQuestion();
	
	String getCorrectValue();
	
	String getAnswers();

}
