/*
 * MazeTest.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package tests;

import static org.junit.Assert.*;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import model.MazeTile;
import model.Movement;
import utilities.MazeSolver;

/**
 * MazeTest is a class which tests methods of the Maze class.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class MazeTest {
	
	/** The random object used for randomized testing */
	private static final Random RAND = new Random();
	
	/** The number of trials performed in iterative test cases */
	private static final int TRIALS = 100;
	
	/** The MazeForTests test fixture */
	private MazeForTests myMaze;

	@Before
	public void setUp() {
		myMaze = new MazeForTests();
	}

	@Test (expected = NullPointerException.class)
	public void testAddPropertyChangeListenerNull() {
		myMaze.addPropertyChangeListener(null);
	}

	@Test
	public void testIsMovementLegal() {
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (int i = 1; i <= TRIALS; i++) {
			myMaze.myCurrTile = tileList.get(RAND.nextInt(tileList.size()));
			for (final Movement move : Movement.values()) {
				assertEquals("isMovementLegal() produced an unexpected result!",
					         myMaze.myTiles.containsKey(myMaze.myCurrTile.getPointForMovement(move)),
					         myMaze.isMovementLegal(move));
			}
		}
	}

	@Test
	public void testAdvanceCurrentTileIllegal() {
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (int i = 1; i <= TRIALS; i++) {
			myMaze.myCurrTile = tileList.get(RAND.nextInt(tileList.size()));
			final List<Movement> movesToTry = new ArrayList<>();
			for (final Movement move : Movement.values()) {
				if (!myMaze.isMovementLegal(move)) {
					movesToTry.add(move);
				}
			}
			for (final Movement move : movesToTry) {
				boolean flag = false;
				try {
					myMaze.advanceCurrentTile(move);
				} catch (final IllegalStateException ex) {
					flag = true;
				}
				assertEquals("advanceCurrentTile() produced an unexpected result with an illegal movement!",
						     true, flag);
			}
		}
	}
	
	@Test
	public void testAdvanceCurrentTileLegal() {
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (int i = 1; i <= TRIALS; i++) {
			myMaze.myCurrTile = tileList.get(RAND.nextInt(tileList.size()));
			final List<Movement> movesToTry = new ArrayList<>();
			for (final Movement move : Movement.values()) {
				if (myMaze.isMovementLegal(move)) {
					movesToTry.add(move);
				}
			}
			for (final Movement move : movesToTry) {
				final MazeTile prevTile = myMaze.myCurrTile;
				myMaze.advanceCurrentTile(move);
				assertEquals("advanceCurrentTile() produced an unexpected result with a legal movement!",
						     prevTile.getPointForMovement(move), myMaze.myCurrTile.getPoint());
				myMaze.advanceCurrentTile(move.getOpposite());
			}
		}
	}
	
	@Test (expected = NullPointerException.class)
	public void testAdvanceCurrentTileNull() {
		myMaze.advanceCurrentTile(null);
	}

	@Test
	public void testHasTavern() {
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (int i = 1; i <= TRIALS; i++) {
			myMaze.myCurrTile = tileList.get(RAND.nextInt(tileList.size()));
			assertEquals("hasTavern() produced an unexpected result!",
					     myMaze.myTaverns.containsKey(myMaze.myCurrTile.getPoint()), myMaze.hasTavern());
		}
	}

	@Test
	public void testHasWater() {
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (int i = 1; i <= TRIALS; i++) {
			myMaze.myCurrTile = tileList.get(RAND.nextInt(tileList.size()));
			assertEquals("hasWater() produced an unexpected result!",
					     myMaze.myWaters.contains(myMaze.myCurrTile.getPoint()), myMaze.hasWater());
		}
	}

	@Test
	public void testRemoveTavernIllegal() {
		myMaze.myTiles.keySet().removeAll(myMaze.myTaverns.keySet());
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (int i = 1; i <= TRIALS; i++) {
		    myMaze.myCurrTile = tileList.get(RAND.nextInt(tileList.size()));
		    boolean flag = false;
		    try {
		    	myMaze.removeTavern();
		    } catch (final IllegalStateException ex) {
		    	flag = true;
		    }
		    assertEquals("removeTavern() produced an unexpected result with an illegal tile!",
		    		     true, flag);
		}
	}

	@Test
	public void testRemoveTavernLegal() {
		myMaze.myTiles.keySet().retainAll(myMaze.myTaverns.keySet());
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (final MazeTile tile : tileList) {
			final int sizeSave = myMaze.myTaverns.size();
		    myMaze.myCurrTile = tile;
		    myMaze.removeTavern();
		    assertEquals("removeTavern() produced an unexpected result with an legal tile!",
		    		     sizeSave - 1, myMaze.myTaverns.size());
		}
	}
	
	@Test
	public void testRemoveWaterIllegal() {
		myMaze.myTiles.keySet().removeAll(myMaze.myWaters);
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (int i = 1; i <= TRIALS; i++) {
		    myMaze.myCurrTile = tileList.get(RAND.nextInt(tileList.size()));
		    boolean flag = false;
		    try {
		    	myMaze.removeWater();
		    } catch (final IllegalStateException ex) {
		    	flag = true;
		    }
		    assertEquals("removeTavern() produced an unexpected result with an illegal tile!",
		    		     true, flag);
		}
	}
	
	@Test
	public void testRemoveWaterLegal() {
		myMaze.myTiles.keySet().retainAll(myMaze.myWaters);
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (final MazeTile tile : tileList) {
			final int sizeSave = myMaze.myWaters.size();
		    myMaze.myCurrTile = tile;
		    myMaze.removeWater();
		    assertEquals("removeTavern() produced an unexpected result with an legal tile!",
		    		     sizeSave - 1, myMaze.myWaters.size());
		}
	}

	@Test
	public void testGetTavernTriviaIllegal() {
		myMaze.myTiles.keySet().removeAll(myMaze.myTaverns.keySet());
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (int i = 1; i <= TRIALS; i++) {
		    myMaze.myCurrTile = tileList.get(RAND.nextInt(tileList.size()));
		    boolean flag = false;
		    try {
		    	myMaze.getTavernTrivia();
		    } catch (final IllegalStateException ex) {
		    	flag = true;
		    }
		    assertEquals("getTavernTrivia() produced an unexpected result with an illegal tile!",
		    		     true, flag);
		}
	}
	
	@Test
	public void testGetTavernTriviaLegal() {
		myMaze.myTiles.keySet().retainAll(myMaze.myTaverns.keySet());
		final List<MazeTile> tileList = new ArrayList<>(myMaze.myTiles.values());
		for (final MazeTile tile : tileList) {
			myMaze.myCurrTile = tile;
		    assertNotEquals("removeTavern() produced an unexpected result with an legal tile!",
		    		        null, myMaze.getTavernTrivia());
		}
	}

	@Test
	public void testGetTileMap() {
		assertTrue("getTileMap() produced an unexpected result!",
				   myMaze.myTiles.equals(myMaze.getTileMap()) &&
				   !(myMaze.myTiles == myMaze.getTileMap()));
	}

	@Test
	public void testGetTileData() {
		final Map<Point, Integer> dataMap = myMaze.getTileData();
		for (final Point pt : myMaze.myTiles.keySet()) {
			assertTrue("getTileData() produced an unexpected result!", 
					   dataMap.containsKey(pt) && dataMap.get(pt) == myMaze.myTiles.get(pt).getImageIndex());
		}
	}

	@Test
	public void testGetWaterPoints() {
		assertTrue("getWaterPoints() produced an unexpected result!",
				   myMaze.myWaters.equals(myMaze.getWaterPoints()) &&
				   !(myMaze.myWaters == myMaze.getWaterPoints()));
	}

	@Test
	public void testGetTavernPoints() {
		assertTrue("getTavernPoints() produced an unexpected result!",
				   myMaze.myTaverns.keySet().equals(myMaze.getTavernPoints()) &&
				   !(myMaze.myTaverns.keySet() == myMaze.getTavernPoints()));
	}
	
	@Test
	public void testCopyPointSet() {
		for (int i = 1; i <= TRIALS; i++) {
			final Set<Point> ptSet = new HashSet<>();
			final int randSize = RAND.nextInt(TRIALS);
			for (int j = 1; j <= randSize; j++) {
				ptSet.add(new Point(RAND.nextInt(), RAND.nextInt()));
			}
			final Set<Point> ptSetCopy = myMaze.copyPointSet(ptSet);
			final List<Point> originalPts = new ArrayList<>(ptSet);
			final List<Point> copiedPts = new ArrayList<>(ptSetCopy);
			boolean result = false;
			if (ptSet.size() == ptSetCopy.size()) {
				Collections.sort(originalPts, (theFirst, theSecond) -> {
					int diff = (int) (theFirst.getX() - theSecond.getX());
					diff = diff == 0 ? (int) (theFirst.getY() - theSecond.getY()) : diff;
					return diff;
				});
				Collections.sort(copiedPts, (theFirst, theSecond) -> {
					int diff = (int) (theFirst.getX() - theSecond.getX());
					diff = diff == 0 ? (int) (theFirst.getY() - theSecond.getY()) : diff;
					return diff;
				});
				result = true;
				for (int index = 0; index < originalPts.size(); index++) {
					result = result && originalPts.get(index).equals(copiedPts.get(index)) &&
							           originalPts.get(index) != copiedPts.get(index);
				}
				assertTrue("copyPointSet() produced an unexpected result!", result);
			}
		}
	}
	
	@Test
	public void testMazeSolveability() {
		for (int i = 1; i <= TRIALS; i++) {
			myMaze = new MazeForTests();
			assertTrue("A maze was not solveable!", MazeSolver.isMazeSolveable(myMaze));
		}
	}

}
