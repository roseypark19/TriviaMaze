package model;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;

import utilities.MazeGenerator;
import utilities.SpriteUtilities;
import utilities.TriviaUtilities;

public class Maze implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 309169416134584707L;
	private static final BufferedImage FLAGS = SpriteUtilities.getFlags();
	private static final BufferedImage WATER = SpriteUtilities.getWater();
	public static final String END_REACHED = "end reached";
	private static final Random RAND = new Random();
	private final PropertyChangeSupport myPcs;
	private final Map<Point, MazeTile> myTiles;
	private final Map<Point, Tavern> myTaverns;
	private final Set<Point> myWaters;
	private MazeTile myCurrTile;
	
	public Maze() {
		myTiles = MazeGenerator.generateTileMap();
		myTaverns = getTavernMap();
		myWaters = getWaterSet();
		myCurrTile = myTiles.get(MazeGenerator.getEntryPoint());
		myPcs = new PropertyChangeSupport(this);
	}
	
	public void addPropertyChangeListener(final String theType,
			                              final PropertyChangeListener theListener) {
		myPcs.addPropertyChangeListener(theType, theListener);
	}
	
	public boolean isMovementLegal(final Movement theMove) {
		return myTiles.containsKey(myCurrTile.getPointForMovement(theMove));
	}
	
	public void advanceCurrentTile(final Movement theMove) {
		if (isMovementLegal(theMove)) {
			myCurrTile = myTiles.get(myCurrTile.getPointForMovement(theMove));
		} else {
			throw new IllegalArgumentException("Tile not present in this movement direction!");
		}
	}
	
	public boolean hasTavern() {
		return myTaverns.containsKey(myCurrTile.getPoint());
	}
	
	public boolean hasWater() {
		return myWaters.contains(myCurrTile.getPoint());
	}
	
	public void checkEndReached() {
		if (myCurrTile.getPoint().equals(MazeGenerator.getExitPoint())) {
			myPcs.firePropertyChange(END_REACHED, false, true);
		}
	}
	
	public void removeTavern() {
		if (!myTaverns.containsKey(myCurrTile.getPoint())) {
			throw new IllegalArgumentException("No tavern on this maze tile!");
		}
		myTaverns.remove(myCurrTile.getPoint());
	}
	
	public void removeWater() {
		if (!myWaters.contains(myCurrTile.getPoint())) {
			throw new IllegalArgumentException("No water on this maze tile!");
		}
		myWaters.remove(myCurrTile.getPoint());
	}
	
	public Trivia getTavernTrivia() {
		if (!hasTavern()) {
			throw new IllegalArgumentException("No tavern on this maze tile!");
		}
		return myTaverns.get(myCurrTile.getPoint()).getTrivia();
	}
	
	public void draw(final Graphics2D theGraphics, final boolean theBaseFinished) {
		if (!theBaseFinished) {
			for (final MazeTile tile : myTiles.values()) {
				tile.draw(theGraphics);
			}
			theGraphics.drawImage(FLAGS, (int) MazeGenerator.getExitPoint().getX() + 1, 
					                     (int) MazeGenerator.getExitPoint().getY(), null);
			for (final Point pt : myWaters) {
				theGraphics.drawImage(WATER, (int) pt.getX() + 12, 
						                     (int) pt.getY() + 6, null);
			}
		} else {
			for (final Tavern tavern : myTaverns.values()) {
				tavern.draw(theGraphics);
			}
		}
	}
	
	private Map<Point, Tavern> getTavernMap() {
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
				tavernMap.put(newPoint, new Tavern(newPoint, triviaList.remove(0)));
				points.add(pt);
			}
			for (final Point pt : points) {
				pt.setLocation(pt.getX(), pt.getY() + 2 * MazeTile.SIZE);
			}
		}
		return tavernMap;
	}
	
	private Set<Point> getWaterSet() {
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
