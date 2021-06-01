/*
 * KeyPadLabel.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.Objects;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;

import model.Movement;


/**
 * KeyPadLabel is a class to display valid and invalid movement directions.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class KeyPadLabel extends JLabel {
	
	/** The serial version UID */
	private static final long serialVersionUID = 6670444409184566253L;
	
	/** The disabled image icon */
	private static final ImageIcon DISABLED = new ImageIcon("playpanel_sprites/redX.png");
	
	/** The font used */
	private static final Font LABEL_FONT = new Font(Font.MONOSPACED, Font.BOLD, 45);
	
	/** The size of a label */
	private static final int SIZE = 70;
	
	/** The movement associated with this label */
	private final Movement myMovement;
	
	/**
	 * Constructs a new KeyPadLabel for a provided Movement. 
	 * 
	 * @param theMove the movement to be assigned to this label
	 * @throws NullPointerException if theMove is null
	 */
	public KeyPadLabel(final Movement theMove) {
		Objects.requireNonNull(theMove, "Movements must be non-null!");
		myMovement = theMove;
		setPreferredSize(new Dimension(SIZE, SIZE));
		setFont(LABEL_FONT);
		setBackground(Color.LIGHT_GRAY);
		setForeground(Color.BLACK);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, 4));
		setFocusable(false);
		setOpaque(true);
		setHorizontalAlignment(JLabel.CENTER);
	}
	
	/**
	 * Updates the appearance of this label according to the true/false enabled parameter.
	 * 
	 * @param theEnabled a true/false indication of whether or not this label should
	 *        be "enabled".
	 */
	public void updateAppearance(final boolean theEnabled) {
		if (theEnabled) {
			setText(myMovement.toString());
			setIcon(null);
		} else {
			setText(null);
			setIcon(DISABLED);
		}
	}
	
	/**
	 * Provides the movement corresponding to this label.
	 * 
	 * @return this label's movement
	 */
	public Movement getMovement() {
		return myMovement;
	}
}
