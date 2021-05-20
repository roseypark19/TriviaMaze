package view;

import javax.swing.JFrame;

public class GameFrame extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = -997412424190795317L;
	private static GameFrame uniqueInstance = new GameFrame();

	private GameFrame() {
		setTitle("Maze Hops");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		add(GameLayers.getInstance());
//		add(MazePanel.getInstance(), BorderLayout.WEST);
//		add(PlayPanel.getInstance(), BorderLayout.EAST);
		pack();
		setLocationRelativeTo(null);
		setResizable(false);
		setVisible(true);
	}
	
	public static synchronized GameFrame getInstance() {
		return uniqueInstance;
	}
}
