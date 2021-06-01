/*
 * GameLauncher.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package program;

import view.GameFrame;

/**
 * GameLauncher is a class which launches the Maze Hops game.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class GameLauncher {
	
	/** Displays a new game frame which hosts the Maze Hops game. */
	private GameLauncher() {
		new GameFrame();
	}
	
	/**
	 * Instantiates a new game launcher which displays the Maze Hops game to the screen.
	 * 
	 * @param theArgs command line arguments - ignored by this program
	 */
	public static void main(final String[] theArgs) {
		new GameLauncher();
	}
}
