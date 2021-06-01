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

public class GamePanel extends JPanel implements PropertyChangeListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5014076795452073479L;
	private final Player myPlayer;
	private final Maze myMaze;
	private final MazePanel myMazePanel;
	private final PlayPanel myPlayPanel;
	private boolean myGameOver;
	private boolean myGameWon;
	
	public GamePanel() {
		myGameOver = false;
		myGameWon = false;
		setLayout(new BorderLayout());
		myPlayer = new Player();
		myMaze = new Maze();
		myMazePanel = new MazePanel(myPlayer, myMaze);
		myPlayPanel = new PlayPanel(myPlayer, myMaze);
		myMazePanel.addPropertyChangeListener(myPlayPanel);
		myPlayPanel.addPropertyChangeListener(myMazePanel);
		myPlayPanel.addPropertyChangeListener(this);
		myMaze.addPropertyChangeListener(this);
		myPlayer.addPropertyChangeListener(this);
		myPlayer.addPropertyChangeListener(myPlayPanel);
		addKeyListener(new KeyboardListener());
		add(myMazePanel, BorderLayout.WEST);
		add(myPlayPanel, BorderLayout.EAST);
		requestFocus();
	}
	
	public boolean isGameWon() {
		return myGameWon;
	}

	public boolean isGameOver() {
		return myGameOver;
	}
	
	public void restoreListeners() {
		myMazePanel.restoreListeners();
		myPlayPanel.restoreListeners();
		myMaze.restoreListeners();
		myPlayer.restoreListeners();
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
	
	private class KeyboardListener extends KeyAdapter {
		
		private boolean myReleased;
		
		public KeyboardListener() {
			myReleased = true;
		}

		@Override
		public void keyTyped(final KeyEvent theEvent) {
			if (myReleased) {
			    myMazePanel.initializeAdvancement(
			    		  Movement.valueof(Character.toUpperCase(theEvent.getKeyChar())));
			    myReleased = false;
			}
		}
		
		@Override
		public void keyReleased(final KeyEvent theEvent) {
			myReleased = true;
		}
	}
}
