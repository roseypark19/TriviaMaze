package view;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.JLayeredPane;
import javax.swing.JPanel;

public class GameLayers extends JLayeredPane {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5014076795452073479L;
	private static GameLayers uniqueInstance = new GameLayers();
	private final JPanel myMainPanel;
	private final JPanel myTitlePanel;
	
	private GameLayers() {
		myMainPanel = new JPanel(new BorderLayout());
		myMainPanel.setBounds(0, 0, MazePanel.WIDTH + PlayPanel.WIDTH, MazePanel.HEIGHT);
		myMainPanel.add(MazePanel.getInstance(), BorderLayout.WEST);
		myMainPanel.add(PlayPanel.getInstance(), BorderLayout.EAST);
		myTitlePanel = TitlePanel.getInstance();
		setPreferredSize(new Dimension(myMainPanel.getWidth(), myMainPanel.getHeight()));
		add(myMainPanel);
		add(myTitlePanel);
		moveToFront(myMainPanel);
	}
	
	public static synchronized GameLayers getInstance() {
		return uniqueInstance;
	}
	
	public void gameOver() {
		this.remove(myMainPanel);
	}

}
