package view;

import java.awt.BorderLayout;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;

import javax.swing.JPanel;

import model.Maze;
import model.Movement;
import model.Player;

public class GamePanel extends JPanel implements PropertyChangeListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5014076795452073479L;
	private boolean myGameOver;
	private boolean myGameWon;
	
	public GamePanel() {
		myGameOver = false;
		myGameWon = false;
		setLayout(new BorderLayout());
		final Player player = new Player();
		final Maze maze = new Maze();
		final MazePanel mazePan = new MazePanel(player, maze);
		final PlayPanel playPan = new PlayPanel(player, maze);
		mazePan.addPropertyChangeListener(playPan);
		playPan.addPropertyChangeListener(mazePan);
		playPan.addPropertyChangeListener(this);
		maze.addPropertyChangeListener(this);
		player.addPropertyChangeListener(this);
		player.addPropertyChangeListener(playPan);
		add(mazePan, BorderLayout.WEST);
		add(playPan, BorderLayout.EAST);
		addKeyListener(new KeyboardListener(mazePan));
		requestFocus();
	}
	
	public boolean isGameWon() {
		return myGameWon;
	}

	public boolean isGameOver() {
		return myGameOver;
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
		
		private final MazePanel myMazePanel;
		private boolean myReleased;
		
		public KeyboardListener(final MazePanel theMazePan) {
			myMazePanel = theMazePan;
			myReleased = true;
		}

		@Override
		public void keyTyped(final KeyEvent theEvent) {
			if (myReleased) {
			    myMazePanel.initializeAdvancement(Movement.valueof(Character.toUpperCase(
													               theEvent.getKeyChar())));
			    myReleased = false;
			}
		}
		
		@Override
		public void keyReleased(final KeyEvent theEvent) {
			myReleased = true;
		}
	}
}
