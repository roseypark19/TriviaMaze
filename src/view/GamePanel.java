package view;

import java.awt.BorderLayout;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;

import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;
import javax.swing.JPanel;

import model.Maze;
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
		player.addPropertyChangeListener(Player.NO_HP, this);
		final Maze maze = new Maze();
		maze.addPropertyChangeListener(Maze.END_REACHED, this);
		final MazePanel mazePan = new MazePanel(player, maze);
		final PlayPanel playPan = new PlayPanel(player, maze);
		final TriviaPanel trivPan = new TriviaPanel(player, maze);
		mazePan.connectPanels(playPan, trivPan);
		playPan.connectPanels(mazePan, trivPan);
		trivPan.connectPanels(mazePan, playPan);
		add(mazePan, BorderLayout.WEST);
		add(playPan, BorderLayout.EAST);
		addFocusListener(new FocusAdapter() {
			
			public void focusGained(final FocusEvent theEvent) {
				mazePan.grabFocus();
			}
			
		});
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
		}
	}
}
