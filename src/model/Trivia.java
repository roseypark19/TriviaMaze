package model;

public interface Trivia {
	
	boolean isCorrect(final String theAnswerKey);
	
	boolean isAnswered();
	
	QuestionType getTriviaType();
	
	String getQuestion();
	
	String getAnswerPrompt();
	
	String getCorrectValue();
	
	String getAnswers();

}
