/*
 * MultiChoiceButton.java
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
 * MultiChoiceButton is a class for selecting answer options to multiple choice trivia
 * questions.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class MultiChoiceButton extends JButton implements TriviaComponent {
	
	/** The serial version UID */
	private static final long serialVersionUID = -8167901072139437724L;
	
	/** The button width */
	private static final int WIDTH = 55;
	
	/** The button height */
	private static final int HEIGHT = 55;
	
	/** The button border width */
	private static final int BORDER_WIDTH = 3;
	
	/** The button font */
	private static final Font FONT = new Font(Font.MONOSPACED, Font.BOLD, 40);
	
	/** The button's choice character */
	private final char myChoice;
	
	/**
	 * Constructs a new MultiChoiceButton with a provided choice character and parent
	 * trivia panel.
	 * 
	 * @param theChoice the choice character
	 * @param theTrivPan the parent trivia panel
	 * @throws NullPointerException if theTrivPan is null
	 */
	public MultiChoiceButton(final char theChoice, final TriviaPanel theTrivPan) {
		super(String.valueOf(theChoice));
		Objects.requireNonNull(theTrivPan, "Trivia panels must be non-null!");
		myChoice = theChoice;
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setBackground(Color.LIGHT_GRAY);
		setForeground(Color.BLACK);
		setFont(FONT);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		setFocusable(false);
		addActionListener(theTrivPan);
	}
	
	/**
	 * Adds an action listener to this button which sends the choice character of this
	 * button to the provided parent trivia panel for processing.
	 * 
	 * @param theTrivPan the parent trivia panel of this button
	 * @throws NullPointerException if theTrivPan is null
	 */
	public void addActionListener(final TriviaPanel theTrivPan) {
		Objects.requireNonNull(theTrivPan, "Trivia panels must be non-null!");
		super.addActionListener(theEvent -> theTrivPan.processResponse(String.valueOf(myChoice)));
	}

}
