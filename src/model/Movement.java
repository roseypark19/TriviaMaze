package model;

public enum Movement {
	
	UP('W'),
	
	LEFT('A'),
	
	DOWN('S'),
	
	RIGHT('D');
	
	private final char myLetter;
	
	Movement(final char theLetter) {
		myLetter = theLetter;
	}
	
	@Override
	public String toString() {
		return String.valueOf(myLetter);
	}

}
