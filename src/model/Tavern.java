package model;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;

import utilities.SpriteUtilities;

public class Tavern {
	
	private static final BufferedImage IMAGE = SpriteUtilities.getTavern();
	private final Point myPoint;
	private final Trivia myTrivia;
	
	public Tavern(final Point thePoint, final Trivia theTrivia) {
		myPoint = new Point(thePoint);
		myTrivia = theTrivia;
	}
	
	public void draw(final Graphics2D theGraphics) {
		theGraphics.drawImage(IMAGE, (int) myPoint.getX() + 1, (int) myPoint.getY(), null);
	}
	
	public Trivia getTrivia() {
		return myTrivia;
	}
}
