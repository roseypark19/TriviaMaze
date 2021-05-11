package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

public class AnswerField extends JTextField {
	
	private static final int WIDTH = 475;
	private static final int HEIGHT = 250;
	private static final int BORDER_WIDTH = 4;
	private static final Font FONT = new Font(Font.MONOSPACED, Font.BOLD, 25);
	
	public AnswerField() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setEditable(false);
		setFocusable(false);
		setFont(FONT);
		setForeground(Color.BLACK);
		setText("Answers go here.");
		setHorizontalAlignment(SwingConstants.CENTER);
		setAlignmentY(CENTER_ALIGNMENT);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
	}

}
