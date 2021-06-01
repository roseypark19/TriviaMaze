/*
 * SoundType.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

/**
 * SoundType is an enum to represent different audio sounds in the Maze Hops game. 
 * For simplicity, the string for each sound type corresponds to its audio file.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public enum SoundType {
	
	/** The song played on the game's title screen */
	TITLE("sound_files/RedSoloCup.wav"),

	/** The song played while playing the game */
	BACKGROUND("sound_files/gamemusic.wav"),
	
	/** The sound played when a correct trivia response is entered */
	CORRECT("sound_files/correct.wav"),
	
	/** The sound played when an incorrect trivia response is entered */
	INCORRECT("sound_files/fail.wav"),
	
	/** The sound played when the player walks over a water */
	DRINK("sound_files/water.wav"),
	
	/** The sound played when a game is won */
	WIN("sound_files/wingame.wav"),
	
	/** The sound played when a game is lost */
	LOSE("sound_files/losegame.wav");
	
	/** The string file name assigned to this sound type */
	private final String myFileName;
	
	/**
	 * Constructs a new SoundType with the specified string file name.
	 * 
	 * @param theFileName the file name to be assigned
	 */
	SoundType(final String theFileName) {
		myFileName = theFileName;
	}
	
	/**
	 * Provides a string representation of this sound type. The string representation of a
	 * sound type simply returns the corresponding file name. 
	 * 
	 * @return the string representation of this sound type
	 */
	@Override
	public String toString() {
		return myFileName;
	}
}
