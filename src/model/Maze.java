package model;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import utilities.MazeGenerator;
import utilities.SpriteUtilities;

public class Maze {
	
	private static final BufferedImage FLAGS = SpriteUtilities.getFlags();
	private static final BufferedImage WATER = SpriteUtilities.getWater();
	private static final Random RAND = new Random();
	private static Maze uniqueInstance = new Maze();
	private final Map<Point, MazeTile> myTiles;
	private final Map<Point, Tavern> myTaverns;
	private final Set<Point> myWaters;
	private MazeTile myCurrTile;
	
	private Maze() {
		myTiles = MazeGenerator.generateTileMap();
		myTaverns = getTavernMap();
		myWaters = getWaterSet();
		myCurrTile = myTiles.get(MazeGenerator.getEntryPoint());
	}
	
	public static synchronized Maze getInstance() {
		return uniqueInstance;
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
		final Map<Point, Tavern> tavernMap = new HashMap<>();
		final List<Point> points = getAlternatingFirstRow();
		for (int row = 0; row < MazeGenerator.SIZE; row += 2) {
			final List<Point> randPts = new ArrayList<>();
			for (int count = 1; count <= 3; count++) {
				randPts.add(points.remove(RAND.nextInt(points.size())));
			}
			for (final Point pt : randPts) {
				final Point newPoint = new Point(pt);
				tavernMap.put(newPoint, new Tavern(newPoint));
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
