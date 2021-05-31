package model;

import java.awt.Point;
import java.io.Serializable;
import java.util.Objects;
import java.util.Random;

import utilities.MazeGenerator;

public class MazeTile implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1423349819693002944L;
	private static final Random RAND = new Random();
	private static final int UNIQUE_IMAGES = 5;
	public static final int SIZE = 48;
	private static int uniqueID;
	private final Point myPoint;
	private final int myImageIndex;
	private int myID;
	
	public MazeTile(final Point thePoint) {
		Objects.requireNonNull(thePoint, "Points must be non-null!");
		myPoint = new Point(thePoint);
		myImageIndex = RAND.nextInt(UNIQUE_IMAGES);
		myID = uniqueID++ % ((MazeGenerator.SIZE * MazeGenerator.SIZE) + 2);
	}
	
	public Point getPointForMovement(final Movement theMove) {
		Objects.requireNonNull(theMove, "Movements must be non-null!");
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
	
	public int getID() {
		return myID;
	}
	
	public int getImageIndex() {
		return myImageIndex;
	}
	
	public Point getPoint() {
		return new Point(myPoint);
	}
}
