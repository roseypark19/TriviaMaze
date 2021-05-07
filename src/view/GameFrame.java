package view;

import java.awt.BorderLayout;

import javax.swing.JFrame;

public class GameFrame extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = -997412424190795317L;

	public GameFrame() {
		setTitle("Maze Hops");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		add(MazePanel.getInstance());
		add(PlayPanel.getInstance(), BorderLayout.EAST);
		pack();
		setLocationRelativeTo(null);
		setResizable(false);
		setVisible(true);
	}
}
