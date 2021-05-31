package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.Objects;

import javax.swing.BorderFactory;
import javax.swing.JTextField;

import view.PlayPanel.TriviaPanel;

public class ShortAnswerField extends JTextField implements TriviaComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 3339069410871811557L;
	private static final int WIDTH = 350;
	private static final int HEIGHT = 55;
	private static final Font FONT = new Font(Font.MONOSPACED, Font.PLAIN, 30);
	private static final int BORDER_WIDTH = 3;
	
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
	
	public void addActionListener(final TriviaPanel theTrivPan) {
		super.addActionListener(theEvent -> {
			theTrivPan.processResponse(getText());
			setText(null);
		});
	}
}
