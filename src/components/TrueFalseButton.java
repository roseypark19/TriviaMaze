package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JButton;

public class TrueFalseButton extends JButton {
	
	private static final int WIDTH = 110;
	private static final int HEIGHT = 45;
	private static final Font FONT = new Font(Font.MONOSPACED, Font.BOLD, 30);
	private static final int BORDER_WIDTH = 3;
	private final boolean myBool;
	
	public TrueFalseButton(final boolean theBoolean) {
		super(String.valueOf(theBoolean));
		myBool = theBoolean;
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setBackground(Color.LIGHT_GRAY);
		setForeground(Color.BLACK);
		setFont(FONT);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		setFocusable(false);
		// action listener to be added once QA functionality is implemented
	}

}