package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JButton;

public class MultiChoiceButton extends JButton {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -8167901072139437724L;
	private static final int WIDTH = 55;
	private static final int HEIGHT = 55;
	private static final int BORDER_WIDTH = 3;
	private static final Font FONT = new Font(Font.MONOSPACED, Font.BOLD, 40);
	private final char myChoice;
	
	public MultiChoiceButton(final char theChoice) {
		super(String.valueOf(theChoice));
		myChoice = theChoice;
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setBackground(Color.LIGHT_GRAY);
		setForeground(Color.BLACK);
		setFont(FONT);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		setFocusable(false);
		// action listener to be added once QA functionality is implemented
	}

}
