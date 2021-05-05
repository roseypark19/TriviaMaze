package model;

public enum Movement {
	
	RIGHT('D'),
	
	LEFT('A'),
	
	UP('W'),
	
	DOWN('S');
	
	private final char myLetter;
	
	Movement(final char theLetter) {
		myLetter = theLetter;
	}
	
	@Override
	public String toString() {
		return String.valueOf(myLetter);
	}

}
