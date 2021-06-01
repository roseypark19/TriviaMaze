/*
 * MazeTile.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

import java.awt.Point;
import java.io.Serializable;
import java.util.Objects;
import java.util.Random;

import utilities.MazeGenerator;

/**
 * MazeTile is a class which represents an individual tile of a maze and provides
 * specific location-based data.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class MazeTile implements Serializable {

	/** The serial version UID */
	private static final long serialVersionUID = -1423349819693002944L;
	
	/** The random object used for generating image indices */
	private static final Random RAND = new Random();
	
	/** The number of unique tile images */
	private static final int UNIQUE_IMAGES = 5;
	
	/** The size of a tile side */
	public static final int SIZE = 48;
	
	/** A unique tile ID number assigned to each new tile */
	private static int uniqueID;
	
	/** The point location of this tile */
	private final Point myPoint;
	
	/** The sprite image index of this tile */
	private final int myImageIndex;
	
	/** The ID number of this tile */
	private int myID;
	
	/**
	 * Constructs a new MazeTile at the provided point location.
	 * 
	 * @param thePoint the point location for the tile
	 * @throws NullPointerException if thePoint is null
	 */
	public MazeTile(final Point thePoint) {
		Objects.requireNonNull(thePoint, "Points must be non-null!");
		myPoint = new Point(thePoint);
		myImageIndex = RAND.nextInt(UNIQUE_IMAGES);
		myID = uniqueID++ % ((MazeGenerator.SIZE * MazeGenerator.SIZE) + 2);
	}
	
	/**
	 * Retrieves the point of the tile adjacent to this tile in the specified movement 
	 * direction.
	 * 
	 * @param theMove the movement direction
	 * @return the point of the adjacent tile
	 */
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
	
	/**
	 * Provides this tile's ID number.
	 * 
	 * @return the ID number
	 */
	public int getID() {
		return myID;
	}
	
	/**
	 * Provides this tile's image index.
	 * 
	 * @return the image index
	 */
	public int getImageIndex() {
		return myImageIndex;
	}
	
	/**
	 * Provides a copy of this tile's point location.
	 * 
	 * @return the point location
	 */
	public Point getPoint() {
		return new Point(myPoint);
	}
}
