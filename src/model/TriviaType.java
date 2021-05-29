package model;

public enum TriviaType {
	
	MULTICHOICE("MC"),
	
	TRUEFALSE("TF"),
	
	SHORTANSWER("SA");
	
	private final String myType;
	
	TriviaType(final String theType) {
		myType = theType;
	}

}
