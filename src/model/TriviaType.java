package model;

import java.io.Serializable;

public enum TriviaType implements Serializable {
	
	MULTICHOICE("MC"),
	
	TRUEFALSE("TF"),
	
	SHORTANSWER("SA");
	
	private final String myType;
	
	TriviaType(final String theType) {
		myType = theType;
	}

}
