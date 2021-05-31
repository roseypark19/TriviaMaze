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

public class Maze implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 309169416134584707L;
	public static final String END_REACHED = "end reached";
	private static final Random RAND = new Random();
	private final PropertyChangeSupport myPcs;
	private final Map<Point, MazeTile> myTiles;
	private final Map<Point, Tavern> myTaverns;
	private final Set<Point> myWaters;
	private final Timer myNotificationTimer;
	private MazeTile myCurrTile;
	
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
	
	public void addPropertyChangeListener(final PropertyChangeListener theListener) {
		Objects.requireNonNull(theListener, "Property change listeners must be non-null!");
		myPcs.addPropertyChangeListener(theListener);
	}
	
	public void restoreListeners() {
		myNotificationTimer.addActionListener(theEvent -> notifyEndReached());
	}
	
	public boolean isMovementLegal(final Movement theMove) {
		Objects.requireNonNull(theMove, "Movements must be non-null!");
		return myTiles.containsKey(myCurrTile.getPointForMovement(theMove));
	}
	
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
	
	public boolean hasTavern() {
		return myTaverns.containsKey(myCurrTile.getPoint());
	}
	
	public boolean hasWater() {
		return myWaters.contains(myCurrTile.getPoint());
	}
	
	public void removeTavern() {
		if (!myTaverns.containsKey(myCurrTile.getPoint())) {
			throw new IllegalStateException("No tavern on this maze tile!");
		}
		myTaverns.remove(myCurrTile.getPoint());
	}
	
	public void removeWater() {
		if (!myWaters.contains(myCurrTile.getPoint())) {
			throw new IllegalStateException("No water on this maze tile!");
		}
		myWaters.remove(myCurrTile.getPoint());
	}
	
	public Trivia getTavernTrivia() {
		if (!hasTavern()) {
			throw new IllegalStateException("No tavern on this maze tile!");
		}
		return myTaverns.get(myCurrTile.getPoint()).getTrivia();
	}
	
	public Map<Point, MazeTile> getTileMap() {
		final Set<Point> copySet = copyPointSet(myTiles.keySet());
		final Map<Point, MazeTile> tileMapCopy = new HashMap<>();
		for (final Point pt : copySet) {
			tileMapCopy.put(pt, myTiles.get(pt));
		}
		return tileMapCopy;
	}
	
	public Map<Point, Integer> getTileData() {
		final Set<Point> copySet = copyPointSet(myTiles.keySet());
		final Map<Point, Integer> dataMap = new HashMap<>();
		for (final Point pt : copySet) {
			dataMap.put(pt, myTiles.get(pt).getImageIndex());
		}
		return dataMap;
	}
	
	public Set<Point> getWaterPoints() {
		return copyPointSet(myWaters);
	}
	
	public Set<Point> getTavernPoints() {
		return copyPointSet(myTaverns.keySet());
	}
	
	private Set<Point> copyPointSet(final Set<Point> thePointSet) {
		final Set<Point> copySet = new HashSet<>();
		for (final Point pt : thePointSet) {
			copySet.add(new Point((int) pt.getX(), (int) pt.getY()));
		}
		return copySet;
	}
	
	private void notifyEndReached() {
		myPcs.firePropertyChange(END_REACHED, false, true);
	}
	
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
