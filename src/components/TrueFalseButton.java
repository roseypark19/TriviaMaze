/*
 * TrueFalseButton.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.Objects;

import javax.swing.BorderFactory;
import javax.swing.JButton;

import view.PlayPanel.TriviaPanel;

/**
 * TrueFalseButton is a class for entering answers to true/false trivia questions
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class TrueFalseButton extends JButton implements TriviaComponent {
	
	/** The serial version UID */
	private static final long serialVersionUID = 731396828680626895L;
	
	/** The button width */
	private static final int WIDTH = 120;
	
	/** The button height */
	private static final int HEIGHT = 55;
	
	/** The button font */
	private static final Font FONT = new Font(Font.MONOSPACED, Font.BOLD, 35);
	
	/** The button border width */
	private static final int BORDER_WIDTH = 3;
	
	/** The button's choice boolean */
	private final boolean myBool;
	
	/**
	 * Constructs a new TrueFalseButton with a provided choice boolean and parent
	 * trivia panel.
	 * 
	 * @param theChoice the choice boolean
	 * @param theTrivPan the parent trivia panel
	 * @throws NullPointerException if theTrivPan is null
	 */
	public TrueFalseButton(final boolean theBoolean, final TriviaPanel theTrivPan) {
		super(String.valueOf(theBoolean));
		Objects.requireNonNull(theTrivPan, "Trivia panels must be non-null!");
		myBool = theBoolean;
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setBackground(Color.LIGHT_GRAY);
		setForeground(Color.BLACK);
		setFont(FONT);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		setFocusable(false);
		addActionListener(theTrivPan);
	}
	
	@Override
	public void addActionListener(final TriviaPanel theTrivPan) {
		Objects.requireNonNull(theTrivPan, "Trivia panels must be non-null!");
		super.addActionListener(theEvent -> theTrivPan.processResponse(String.valueOf(myBool)));
	}

}
