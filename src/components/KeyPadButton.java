package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;

import model.Movement;
import view.MazePanel;

public class KeyPadButton extends JButton {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 6670444409184566253L;
	private static final ImageIcon DISABLED = new ImageIcon("redX.png");
	private static final Font BUTTON_FONT = new Font(Font.MONOSPACED, Font.BOLD, 45);
	private static final int SIZE = 70;
	private final Movement myMovement;
	
	public KeyPadButton(final Movement theMove, final MazePanel theMazePan) {
		myMovement = theMove;
		setPreferredSize(new Dimension(SIZE, SIZE));
		setFont(BUTTON_FONT);
		setBackground(Color.LIGHT_GRAY);
		setForeground(Color.BLACK);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, 4));
		setFocusable(false);
		addActionListener(theEvent -> theMazePan.initializeAdvancement(myMovement));
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
