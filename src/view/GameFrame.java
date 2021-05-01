package view;

import javax.swing.JFrame;

public class GameFrame extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = -997412424190795317L;

	public GameFrame() {
		setTitle("Maze Hops");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setContentPane(new GamePanel());
		pack();
		setLocationRelativeTo(null);
		setResizable(false);
		setVisible(true);
	}
}
