package model;

import java.awt.Color;
import java.awt.Point;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.Map;

public class MazeTile extends Rectangle2D.Double {

	public static final Color COLOR = Color.LIGHT_GRAY;
	public static final int SIZE = 48;
	private static int uniqueID;
	private int myID;
	private final Map<Movement, MazeTile> myNeighbors;
	
	public MazeTile(final Point thePoint) {
		super(thePoint.getX(), thePoint.getY(), SIZE, SIZE);
		myNeighbors = new HashMap<>();
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
	
	protected void addNeighbor(final Movement theMove, final MazeTile theNeighbor) {
		myNeighbors.put(theMove, theNeighbor);
	}
	
//	protected MazeTile getNeighborForMovement(final Movement theMove) {
//		return myNeighbors.get(theMove);
//	}
	
	public int getID() {
		return myID;
	}
	
	public Point getPoint() {
		return new Point((int) getX(), (int) getY());
	}
	
	public boolean hasNeighborForMovement(final Movement theMove) {
		return myNeighbors.containsKey(theMove);
	}
}
