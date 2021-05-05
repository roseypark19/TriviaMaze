package view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.JButton;

import model.Movement;

public class KeyPadButton extends JButton {
	
	private static final Font BUTTON_FONT = new Font("ButtonFont", Font.BOLD, 27);
	private static final Color BUTTON_COLOR = Color.LIGHT_GRAY;
	private static final int SIZE = 60;
	private final Movement myMovement;
	
	public KeyPadButton(final Movement theMove) {
		super(theMove.toString());
		myMovement = theMove;
		setPreferredSize(new Dimension(SIZE, SIZE));
		setFont(BUTTON_FONT);
		setBackground(BUTTON_COLOR);
	}
	
	public Movement getMovement() {
		return myMovement;
	}
}
