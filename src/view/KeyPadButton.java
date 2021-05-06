package view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;

import model.Movement;

public class KeyPadButton extends JButton {
	
	private static final ImageIcon DISABLED = new ImageIcon("redX.png");
	private static final Color BUTTON_COLOR = new Color(217, 179, 130);
	private static final Font BUTTON_FONT = new Font("ButtonFont", Font.BOLD, 30);
	private static final int SIZE = 70;
	private final Movement myMovement;
	
	public KeyPadButton(final Movement theMove) {
		super();
		myMovement = theMove;
		setPreferredSize(new Dimension(SIZE, SIZE));
		setFont(BUTTON_FONT);
		setBackground(BUTTON_COLOR);
		setForeground(Color.BLACK);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, 4));
		setFocusable(false);
	}
	
	public void updateAppearance(final boolean theEnabled) {
		if (theEnabled) {
			setText(myMovement.toString());
			setIcon(null);
		} else {
			setText(null);
			setIcon(DISABLED);
		}
	}
	
	public Movement getMovement() {
		return myMovement;
	}
}
