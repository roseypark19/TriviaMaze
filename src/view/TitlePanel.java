package view;

import javax.swing.JPanel;

public class TitlePanel extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8276234955438867616L;
	private static TitlePanel uniqueInstance = new TitlePanel();
	// add images for welcome and game over later on
	
	private TitlePanel() {
		setBounds(0, 0, MazePanel.WIDTH + PlayPanel.WIDTH, MazePanel.HEIGHT);
	}
	
	public static synchronized TitlePanel getInstance() {
		return uniqueInstance;
	}

}
