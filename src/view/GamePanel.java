/*
 * GamePanel.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package view;

import java.awt.BorderLayout;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JPanel;

import model.Maze;
import model.Movement;
import model.Player;

/**
 * GamePanel is a class which contains the maze panel and play panel of the Maze Hops game.
 * A game panel also listens for bound property changes of the player and maze to determine
 * if a game is won/lost.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class GamePanel extends JPanel implements PropertyChangeListener {

	/** The serial version UID */
	private static final long serialVersionUID = -5014076795452073479L;
	
	/** This game panel's maze panel */
	private final MazePanel myMazePanel;
	
	/** This game panel's play panel */
	private final PlayPanel myPlayPanel;
	
	/** Indicates whether or not this game has been lost */
	private boolean myGameOver;
	
	/** Indicates whether or not this game has been won */
	private boolean myGameWon;
	
	/** Constructs a new GamePanel and configures necessary property change listeners. */
	public GamePanel() {
		myGameOver = false;
		myGameWon = false;
		setLayout(new BorderLayout());
		setBounds(0, 0, MazePanel.WIDTH + PlayPanel.WIDTH, MazePanel.HEIGHT);
		final Player player = new Player();
		final Maze maze = new Maze();
		myMazePanel = new MazePanel(player, maze);
		myPlayPanel = new PlayPanel(player, maze);
		myMazePanel.addPropertyChangeListener(myPlayPanel);
		myPlayPanel.addPropertyChangeListener(myMazePanel);
		myPlayPanel.addPropertyChangeListener(this);
		maze.addPropertyChangeListener(this);
		player.addPropertyChangeListener(this);
		player.addPropertyChangeListener(myPlayPanel);
		addKeyListener(new KeyboardListener());
		add(myMazePanel, BorderLayout.WEST);
		add(myPlayPanel, BorderLayout.EAST);
		requestFocus();
	}
	
	/**
	 * Indicates whether or not this game is won.
	 * 
	 * @return true if this game is won, false otherwise
	 */
	public boolean isGameWon() {
		return myGameWon;
	}

	/**
	 * Indicates whether or not this game is lost.
	 * 
	 * @return true if this game is lost, false otherwise
	 */
	public boolean isGameOver() {
		return myGameOver;
	}
	
	/** Restores all listeners associated with this game panel and its child panels. */
	public void restoreListeners() {
		myMazePanel.restoreListeners();
		myPlayPanel.restoreListeners();
		addKeyListener(new KeyboardListener());
	}

	@Override
	public void propertyChange(final PropertyChangeEvent theEvent) {
		if (theEvent.getPropertyName().equals(Player.NO_HP)) {
			myGameOver = true;
			firePropertyChange(Player.NO_HP, false, true);
		} else if (theEvent.getPropertyName().equals(Maze.END_REACHED)) {
			myGameWon = true;
			firePropertyChange(Maze.END_REACHED, false, true);
		} else if (theEvent.getPropertyName().equals(PlayPanel.TRIVIA_ANSWERED)) {
			requestFocus();
		}
	}
	
	/**
	 * KeyboardListener is a class which listens for key events and directs this game panel's
	 * maze panel to initialize an advancement in the corresponding movement direction.
	 * 
	 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
	 * @version 31 May 2021
	 */
	private class KeyboardListener extends KeyAdapter {
		
		/** Indicates whether or not a pressed key has been released */
		private boolean myReleased;
		
		/** Constructs a new KeyboardListener */
		public KeyboardListener() {
			myReleased = true;
		}

		@Override
		public void keyTyped(final KeyEvent theEvent) {
			if (myReleased) {
				final Movement move = 
						        Movement.valueof(Character.toUpperCase(theEvent.getKeyChar()));
				if (move != null) {
					myMazePanel.initializeAdvancement(move);
					myReleased = false;
				}
			}
		}
		
		@Override
		public void keyReleased(final KeyEvent theEvent) {
			myReleased = true;
		}
	}
}
