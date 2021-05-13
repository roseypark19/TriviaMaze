package model;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;

import utilities.SpriteUtilities;

public class Tavern {
	
	public static final int SIZE = 48;
	private static final BufferedImage IMAGE = SpriteUtilities.getTavern();
	private final Point myPoint;
	
	public Tavern(final Point thePoint) {
		myPoint = new Point(thePoint);
	}
	
	public void draw(final Graphics2D theGraphics) {
		theGraphics.drawImage(IMAGE, (int) myPoint.getX() + 1, 
				                     (int) myPoint.getY(), null);
	}
}
