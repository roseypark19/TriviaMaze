package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.Objects;

import javax.swing.BorderFactory;
import javax.swing.JButton;

import view.PlayPanel.TriviaPanel;

public class MultiChoiceButton extends JButton implements TriviaComponent {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -8167901072139437724L;
	private static final int WIDTH = 55;
	private static final int HEIGHT = 55;
	private static final int BORDER_WIDTH = 3;
	private static final Font FONT = new Font(Font.MONOSPACED, Font.BOLD, 40);
	private final char myChoice;
	
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
	
	public void addActionListener(final TriviaPanel theTrivPan) {
		super.addActionListener(theEvent -> theTrivPan.processResponse(String.valueOf(myChoice)));
	}

}
