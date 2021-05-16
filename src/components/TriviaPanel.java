package components;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class TriviaPanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1600296983489784055L;
	// images to be added later
	private static final Color BUBBLE_COLOR = new Color(237, 224, 234);
	private static final int WIDTH = 475;
	private static final int HEIGHT = 595;
	private static final int BORDER_WIDTH = 4;
	private JTextField myTriviaField;
	
	public TriviaPanel() {
		myTriviaField = new JTextField();
		myTriviaField.setEditable(false);
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(false);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		setLayout(new BorderLayout());
	}

}
