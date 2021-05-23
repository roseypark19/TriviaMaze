package view;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JFrame;

import model.Maze;
import model.Player;

public class GameFrame extends JFrame implements PropertyChangeListener {
	
	private GamePanel myGamePanel;

	/**
	 * 
	 */
	private static final long serialVersionUID = -997412424190795317L;

	public GameFrame() {
		setTitle("Maze Hops");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		myGamePanel = new GamePanel();
		myGamePanel.addPropertyChangeListener(this);
		add(myGamePanel);
		pack();
		setLocationRelativeTo(null);
		setResizable(false);
		setVisible(true);
	}

	@Override
	public void propertyChange(final PropertyChangeEvent theEvent) {
		if (theEvent.getPropertyName().equals(Player.NO_HP)) {
			System.out.println("frame knows game is over");
		} else if (theEvent.getPropertyName().equals(Maze.END_REACHED)) {
			System.out.println("frame knows game is won");
		}
	}

}
