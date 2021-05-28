package model;

public enum SoundType {
	
	TITLE("sound_files/RedSoloCup.wav"),

	BACKGROUND("sound_files/LoopMusic.wav"),
	
	CORRECT("sound_files/correct.wav"),
	
	INCORRECT("sound_files/fail.wav"),
	
	DRINK("sound_files/water.wav"),
	
	WIN("sound_files/wingame.wav");
	
	private final String myType;
	
	SoundType(final String theType) {
		myType = theType;
	}
	
	@Override
	public String toString() {
		return myType;
	}
}
