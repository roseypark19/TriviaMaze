package model;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.Random;

import utilities.SpriteUtilities;

public class MazeTile {

//	public static final Color COLOR = new Color(181, 101, 29);
//	private static final BufferedImage IMAGE = SpriteUtilities.getMazeTile();
	private static final BufferedImage[] IMAGES = SpriteUtilities.getMazeTiles();
	private static final Random RAND = new Random();
	public static final int SIZE = 48;
	private static int uniqueID;
	private final Point myPoint;
	private final BufferedImage myImage;
	private int myID;
	
	public MazeTile(final Point thePoint) {
		myPoint = new Point(thePoint);
		myImage = IMAGES[RAND.nextInt(IMAGES.length)];
		myID = uniqueID++;
	}
	
	public Point getPointForMovement(final Movement theMove) {
		Point p = null;
		switch (theMove) {
		case UP:
			p = new Point((int) myPoint.getX(), (int) (myPoint.getY() - SIZE));
			break;
			
		case DOWN:
			p = new Point((int) myPoint.getX(), (int) (myPoint.getY() + SIZE));
			break;
			
		case LEFT:
			p = new Point((int) (myPoint.getX() - SIZE), (int) myPoint.getY());
			break;
			
		case RIGHT:
			p = new Point((int) (myPoint.getX() + SIZE), (int) myPoint.getY());
			break;
		}
		return p;
	}
	
	public void draw(final Graphics2D theGraphics) {
		theGraphics.drawImage(myImage, (int) myPoint.getX(), (int) myPoint.getY(), null);
	}
	
	public int getID() {
		return myID;
	}
	
	public Point getPoint() {
		return new Point(myPoint);
	}
}
