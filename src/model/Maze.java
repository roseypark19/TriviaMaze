/*
 * Maze.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

import java.awt.Point;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Random;
import java.util.Set;

import javax.swing.Timer;

import utilities.MazeGenerator;
import utilities.TriviaUtilities;

/**
 * Maze is a class that implements maze attributes and behaviors used by the Maze Hops game.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class Maze implements Serializable {
	
	/** The serial version UID */
	private static final long serialVersionUID = 309169416134584707L;
	
	/** The bound property indicating the end of the maze being reached */
	public static final String END_REACHED = "end reached";
	
	/** The random object used for tavern and water placing */
	private static final Random RAND = new Random();
	
	/** The property change support which notifies listeners of changes in bound properties */
	private final PropertyChangeSupport myPcs;
	
	/** A point-based mapping of all maze tiles for this maze */
	private final Map<Point, MazeTile> myTiles;
	
	/** A point-based mapping of all taverns for this maze */
	private final Map<Point, Tavern> myTaverns;
	
	/** A point-based mapping of all maze waters for this maze */
	private final Set<Point> myWaters;
	
	/** A timer which delays event firings for changes in bound properties */
	private final Timer myNotificationTimer;
	
	/** The current maze tile being visited for this maze */
	private MazeTile myCurrTile;
	
	/**
	 * Constructs a new maze with a random layout.
	 */
	public Maze() {
		myTiles = MazeGenerator.generateTileMap();
		myTaverns = generateTavernMap();
		myWaters = generateWaterSet();
		myNotificationTimer = new Timer(0, theEvent -> notifyEndReached());
		myNotificationTimer.setInitialDelay(550);
		myNotificationTimer.setRepeats(false);
		myCurrTile = myTiles.get(MazeGenerator.getEntryPoint());
		myPcs = new PropertyChangeSupport(this);
	}
	
	/**
	 * Adds a property change listener to this maze which listens for changes in bound
	 * properties.
	 * 
	 * @param theListener the property change listener to be assigned
	 * @throws NullPointerException if theListener is null
	 */
	public void addPropertyChangeListener(final PropertyChangeListener theListener) {
		Objects.requireNonNull(theListener, "Property change listeners must be non-null!");
		myPcs.addPropertyChangeListener(theListener);
	}
	
	/** Restores the action listeners associated with this maze. */
	public void restoreListeners() {
		myNotificationTimer.addActionListener(theEvent -> notifyEndReached());
	}
	
	/**
	 * Determines if the specified movement results in a valid tile on this maze.
	 * 
	 * @param theMove the desired movement
	 * @return true if the movement is valid, false otherwise
	 * @throws NullPointerException if theMove is null
	 */
	public boolean isMovementLegal(final Movement theMove) {
		Objects.requireNonNull(theMove, "Movements must be non-null!");
		return myTiles.containsKey(myCurrTile.getPointForMovement(theMove));
	}
	
	/**
	 * Advances the current tile of this maze being visited to the tile in the 
	 * specified movement direction.
	 * 
	 * @param theMove the movement direction
	 * @throws NullPointerException if theMove is null
	 * @throws IllegalStateException if this maze does not have a tile in the specified
	 *         movement direction
	 */
	public void advanceCurrentTile(final Movement theMove) {
		Objects.requireNonNull(theMove, "Movements must be non-null!");
		if (isMovementLegal(theMove)) {
			myCurrTile = myTiles.get(myCurrTile.getPointForMovement(theMove));
			if (myCurrTile.getPoint().equals(MazeGenerator.getExitPoint())) {
				myNotificationTimer.start();
			}
		} else {
			throw new IllegalStateException("Tile not present in this movement direction!");
		}
	}
	
	/**
	 * Indicates whether or not this maze has a tavern on its current tile.
	 * 
	 * @return true if a tavern is found, false otherwise
	 */
	public boolean hasTavern() {
		return myTaverns.containsKey(myCurrTile.getPoint());
	}
	
	/**
	 * Indicates whether or not this maze has a water on its current tile.
	 * 
	 * @return true if a water is found, false otherwise
	 */
	public boolean hasWater() {
		return myWaters.contains(myCurrTile.getPoint());
	}
	
	/**
	 * Removes the tavern from this maze's current tile.
	 * 
	 * @throws IllegalStateException if no tavern is found on this maze's current tile
	 */
	public void removeTavern() {
		if (!myTaverns.containsKey(myCurrTile.getPoint())) {
			throw new IllegalStateException("No tavern on this maze tile!");
		}
		myTaverns.remove(myCurrTile.getPoint());
	}
	
	/**
	 * Removes the water from this maze's current tile.
	 * 
	 * @throws IllegalStateException if no water is found on this maze's current tile
	 */
	public void removeWater() {
		if (!myWaters.contains(myCurrTile.getPoint())) {
			throw new IllegalStateException("No water on this maze tile!");
		}
		myWaters.remove(myCurrTile.getPoint());
	}
	
	/**
	 * Provides the trivia for the tavern on this maze's current tile.
	 * 
	 * @return the tavern's trivia
	 * @throws IllegalStateException if no tavern is found on this maze's current tile
	 */
	public Trivia getTavernTrivia() {
		if (!hasTavern()) {
			throw new IllegalStateException("No tavern on this maze tile!");
		}
		return myTaverns.get(myCurrTile.getPoint()).getTrivia();
	}
	
	/**
	 * Provides a copy of this maze's tile map.
	 * 
	 * @return the tile map
	 */
	public Map<Point, MazeTile> getTileMap() {
		final Set<Point> copySet = copyPointSet(myTiles.keySet());
		final Map<Point, MazeTile> tileMapCopy = new HashMap<>();
		for (final Point pt : copySet) {
			tileMapCopy.put(pt, myTiles.get(pt));
		}
		return tileMapCopy;
	}
	
	/**
	 * Provides a mapping of the locations of each maze tile and their corresponding
	 * image indices. 
	 * 
	 * @return the mapping of this maze's tile data
	 */
	public Map<Point, Integer> getTileData() {
		final Set<Point> copySet = copyPointSet(myTiles.keySet());
		final Map<Point, Integer> dataMap = new HashMap<>();
		for (final Point pt : copySet) {
			dataMap.put(pt, myTiles.get(pt).getImageIndex());
		}
		return dataMap;
	}
	
	/**
	 * Provides a copy of this maze's water locations
	 * 
	 * @return the set of water points
	 */
	public Set<Point> getWaterPoints() {
		return copyPointSet(myWaters);
	}
	
	/**
	 * Provides a copy of this maze's tavern locations
	 * 
	 * @return the set of tavern points
	 */
	public Set<Point> getTavernPoints() {
		return copyPointSet(myTaverns.keySet());
	}
	
	/**
	 * Creates a defensive copy of a provided set of points.
	 * 
	 * @param thePointSet the set of points to be copied
	 * @return the copied set of points
	 */
	private Set<Point> copyPointSet(final Set<Point> thePointSet) {
		final Set<Point> copySet = new HashSet<>();
		for (final Point pt : thePointSet) {
			copySet.add(new Point((int) pt.getX(), (int) pt.getY()));
		}
		return copySet;
	}
	
	/**
	 * Notifies all registered property change listeners that the end of this maze has
	 * been reached.
	 */
	private void notifyEndReached() {
		myPcs.firePropertyChange(END_REACHED, false, true);
	}
	
	/**
	 * Generates the point-to-tavern mapping for this maze.
	 * 
	 * @return the point-to-tavern mapping
	 */
	private Map<Point, Tavern> generateTavernMap() {
		final List<Trivia> triviaList = TriviaUtilities.getTriviaList();
		final Map<Point, Tavern> tavernMap = new HashMap<>();
		final List<Point> points = getAlternatingFirstRow();
		for (int row = 0; row < MazeGenerator.SIZE; row += 2) {
			final List<Point> randPts = new ArrayList<>();
			for (int count = 1; count <= 3; count++) {
				randPts.add(points.remove(RAND.nextInt(points.size())));
			}
			for (final Point pt : randPts) {
				final Point newPoint = new Point(pt);
				tavernMap.put(newPoint, new Tavern(triviaList.remove(0)));
				points.add(pt);
			}
			for (final Point pt : points) {
				pt.setLocation(pt.getX(), pt.getY() + 2 * MazeTile.SIZE);
			}
		}
		return tavernMap;
	}
	
	/**
	 * Generates the set points for waters to be placed on this maze.
	 * 
	 * @return the set of water points
	 */
	private Set<Point> generateWaterSet() {
		final Set<Point> waterSet = new HashSet<>();
		final List<Point> points = getAlternatingFirstRow();
		for (int row = 0; row < MazeGenerator.SIZE; row += 2) {
			final List<Point> randPts = new ArrayList<>();
			for (int count = 1; count <= 2; count++) {
				Point trialPt = points.remove(RAND.nextInt(points.size()));
				while (myTaverns.containsKey(trialPt)) {
					final Point tempPt = trialPt;
					trialPt = points.remove(RAND.nextInt(points.size()));
					points.add(tempPt);
				}
				randPts.add(trialPt);
			}
			for (final Point pt : randPts) {
				waterSet.add(new Point(pt));
				points.add(pt);
			}
			for (final Point pt : points) {
				pt.setLocation(pt.getX(), pt.getY() + 2 * MazeTile.SIZE);
			}
		}
		return waterSet;
	}
	
	/**
	 * Provides an alternating first row of points for placing/spacing taverns and waters 
	 * on this maze.
	 * 
	 * @return the list of alternating points
	 */
	private List<Point> getAlternatingFirstRow() {
		final List<Point> points = new LinkedList<>();
		points.add(MazeGenerator.getStartPoint());
		for (int count = 1; count <= MazeGenerator.SIZE / 2; count++) {
			final Point prevPt = points.get(points.size() - 1);
			points.add(new Point((int) prevPt.getX() + 2 * MazeTile.SIZE, 
					             (int) prevPt.getY()));
		}
		return points;
	}

}
