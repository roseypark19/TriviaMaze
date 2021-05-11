package model;

public enum QuestionType {
	
	MULTICHOICE("MC"),
	
	TRUEFALSE("TF"),
	
	NONE("N"),
	
	SHORTANSWER("SA");
	
	private final String myType;
	
	QuestionType(final String theType) {
		myType = theType;
	}

}
