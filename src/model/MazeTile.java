package model;

import java.awt.Color;
import java.awt.Point;
import java.awt.geom.Rectangle2D;

public class MazeTile extends Rectangle2D.Double {

	public static final Color COLOR = new Color(181, 101, 29);
	public static final int SIZE = 48;
	private static int uniqueID;
	private int myID;
	
	public MazeTile(final Point thePoint) {
		super(thePoint.getX(), thePoint.getY(), SIZE, SIZE);
		myID = uniqueID++;
	}
	
	public Point getPointForMovement(final Movement theMove) {
		Point p = null;
		switch (theMove) {
		case UP:
			p = new Point((int) getX(), (int) (getY() - getHeight()));
			break;
			
		case DOWN:
			p = new Point((int) getX(), (int) (getY() + getHeight()));
			break;
			
		case LEFT:
			p = new Point((int) (getX() - getWidth()), (int) getY());
			break;
			
		case RIGHT:
			p = new Point((int) (getX() + getWidth()), (int) getY());
			break;
		}
		return p;
	}
	
	public int getID() {
		return myID;
	}
	
	public Point getPoint() {
		return new Point((int) getX(), (int) getY());
	}
}
