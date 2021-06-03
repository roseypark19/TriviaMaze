/*
 * Movement.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

/**
 * Movement is an enum to represent different player movement directions in the Maze Hops
 * game. The character for each movement type corresponds to the directions of the WASD
 * keys.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public enum Movement {
	
	/** Moves the player upward */
	UP('W'),
	
	/** Moves the player leftward */
	LEFT('A'),
	
	/** Moves the player downward */
	DOWN('S'),
	
	/** Moves the player rightward */
	RIGHT('D');
	
	/** The character assigned to this movement */
	private final char myLetter;
		
	/**
	 * Constructs a new Movement with the specified character.
	 * 
	 * @param theLetter the character to be assigned
	 */
	Movement(final char theLetter) {
		myLetter = theLetter;
	}
	
	/**
	 * Provides a string representation of this movement. The string representation of a
	 * movement simply returns the corresponding WASD key pad character. 
	 * 
	 * @return the string representation of this movement
	 */
	@Override
	public String toString() {
		return String.valueOf(myLetter);
	}
	
	/**
	 * Retrieves the movement in the opposite direction of this movement.
	 * 
	 * @return the movement in the opposite direction
	 */
	public Movement getOpposite() {
		Movement opposite = null;
		switch (myLetter) {
			case 'W':
				opposite = DOWN;
				break;
			case 'A':
				opposite = RIGHT;
				break;
			case 'S':
				opposite = UP;
				break;
			case 'D':
				opposite = LEFT;
				break;
		}
		return opposite;
	}
	
	/**
	 * Retrieves the movement corresponding to the provided character. Note that this
	 * method is case sensitive.
	 * 
	 * @param theChar the character in question
	 * @return the corresponding movement direction if such exists, null otherwise
	 */
	public static Movement valueof(final char theChar) {
		Movement move = null;
		for (final Movement movement : Movement.values()) {
			if (movement.myLetter == theChar) {
				move = movement;
				break;
			}
		}
		return move;
	}

}
