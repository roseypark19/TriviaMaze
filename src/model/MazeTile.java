package model;

import java.awt.Color;
import java.awt.Point;
import java.awt.geom.Rectangle2D;
import java.util.HashSet;
import java.util.Set;

public class MazeTile extends Rectangle2D.Double {

	public static final Color COLOR = Color.LIGHT_GRAY;
	private final Set<MazeTile> myNeighbors;
//	private final Movement myAdjacent;
	
	public MazeTile(final double theX, final double theY) {
		super(theX, theY, 48, 48);
		myNeighbors = new HashSet<>();
//		myAdjacent = theAdj;
	}
	
//	public Movement getAdjacent() {
//		return myAdjacent;
//	}
	
	protected void addNeighbor(final MazeTile theNeighbor) {
		myNeighbors.add(theNeighbor);
	}
	
	public Point getCornerPointForMovement(final Movement theMove) {
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
}
