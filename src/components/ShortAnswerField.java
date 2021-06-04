/*
 * ShortAnswerField.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.Objects;

import javax.swing.BorderFactory;
import javax.swing.JTextField;

import view.PlayPanel.TriviaPanel;

/**
 * ShortAnswerField is a class for entering answers to short answer trivia questions.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class ShortAnswerField extends JTextField implements TriviaComponent {

	/** The serial version UID */
	private static final long serialVersionUID = 3339069410871811557L;
	
	/** The field width */
	private static final int WIDTH = 350;
	
	/** The field height */
	private static final int HEIGHT = 55;
	
	/** The field font */
	private static final Font FONT = new Font(Font.MONOSPACED, Font.PLAIN, 30);
	
	/** The field border width */
	private static final int BORDER_WIDTH = 3;
	
	/**
	 * Constructs a new ShortAnswerField with a provided parent trivia panel.
	 * 
	 * @param theTrivPan the parent trivia panel
	 */
	public ShortAnswerField(final TriviaPanel theTrivPan) {
		Objects.requireNonNull(theTrivPan, "Trivia panels must be non-null!");
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		setEditable(true);
		setForeground(Color.BLACK);
		setFont(FONT);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		setHorizontalAlignment(JTextField.CENTER);
		addActionListener(theTrivPan);
	}
	
	@Override
	public void addActionListener(final TriviaPanel theTrivPan) {
		Objects.requireNonNull(theTrivPan, "Triva panels must be non-null!");
		super.addActionListener(theEvent -> {
			theTrivPan.processResponse(getText().trim());
			setText(null);
		});
	}
}
