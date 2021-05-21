package view;

import java.awt.BorderLayout;

import javax.swing.JPanel;

import model.Maze;
import model.Player;

public class GamePanel extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5014076795452073479L;
	
	public GamePanel() {
		setLayout(new BorderLayout());
		final Player player = new Player();
		final Maze maze = new Maze();
		final MazePanel mazePan = new MazePanel(player, maze);
		final PlayPanel playPan = new PlayPanel(player, maze);
		final TriviaPanel trivPan = new TriviaPanel(player, maze);
		mazePan.connectPanels(playPan, trivPan);
		playPan.connectPanels(mazePan, trivPan);
		trivPan.connectPanels(mazePan, playPan);
		add(mazePan, BorderLayout.WEST);
		add(playPan, BorderLayout.EAST);
	}

}
