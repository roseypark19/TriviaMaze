package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JButton;

import view.TriviaPanel;

public class TrueFalseButton extends JButton {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 731396828680626895L;
	private static final int WIDTH = 120;
	private static final int HEIGHT = 55;
	private static final Font FONT = new Font(Font.MONOSPACED, Font.BOLD, 35);
	private static final int BORDER_WIDTH = 3;
	private final boolean myBool;
	
	public TrueFalseButton(final boolean theBoolean, final TriviaPanel theTrivPan) {
		super(String.valueOf(theBoolean));
		myBool = theBoolean;
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setBackground(Color.LIGHT_GRAY);
		setForeground(Color.BLACK);
		setFont(FONT);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		setFocusable(false);
		addActionListener(theEvent -> theTrivPan.processResponse(String.valueOf(myBool)));
	}

}
