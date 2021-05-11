package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JTextField;

public class ShortAnswerField extends JTextField {

	private static final int WIDTH = 300;
	private static final int HEIGHT = 45;
	private static final Font FONT = new Font(Font.MONOSPACED, Font.PLAIN, 30);
	private static final int BORDER_WIDTH = 3;
	
	public ShortAnswerField() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		setEditable(true);
		setForeground(Color.BLACK);
		setFont(FONT);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		// action listener to be added once QA functionality is implemented
	}
}