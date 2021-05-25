package model;

public enum SoundType {
	
	TITLE("RedSoloCup.wav"),

	BACKGROUND("LoopMusic.wav"),
	
	CORRECT("correct.wav"),
	
	INCORRECT("fail.wav"),
	
	DRINK("water.wav"),
	
	WIN("wingame.wav");
	
	private final String myType;
	
	SoundType(final String theType) {
		myType = theType;
	}
	
	@Override
	public String toString() {
		return myType;
	}
}
