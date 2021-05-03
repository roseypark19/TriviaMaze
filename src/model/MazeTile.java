package model;

import java.awt.Color;
import java.awt.Point;
import java.awt.geom.Rectangle2D;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class MazeTile extends Rectangle2D.Double {

	public static final Color COLOR = Color.LIGHT_GRAY;
	public static final int SIZE = 48;
	private final Map<Movement, MazeTile> myNeighbors;
	
	public MazeTile(final double theX, final double theY) {
		super(theX, theY, SIZE, SIZE);
		myNeighbors = new HashMap<>();
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
	
	public boolean hasNeighborForMovement(final Movement theMove) {
		return myNeighbors.containsKey(theMove);
	}
}
