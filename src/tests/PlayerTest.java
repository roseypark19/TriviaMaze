/*
 * PlayerTest.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package tests;

import static org.junit.Assert.*;

import java.awt.Point;

import org.junit.Before;
import org.junit.Test;

import model.Movement;
import model.Player;
import utilities.MazeGenerator;

/**
 * PlayerTest is a class which tests methods of the Player class.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class PlayerTest {
	
	/** The number of trials performed in iterative test cases */
	private static final int TRIALS = 50;
	
	/** The player text fixture */
	private Player myPlayer;

	/**
	 * Instantiates a new player prior to each test case. Also indirectly tests
	 * {@link Player#Player()}.
	 */
	@Before
	public void setUp() {
		myPlayer = new Player();
	}

	/** 
	 * Test method for {@link Player#addPropertyChangeListener(java.beans.PropertyChangeListener)}
	 * with a null argument.
	 */
	@Test (expected = NullPointerException.class)
	public void testAddPropertyChangeListenerNull() {
		myPlayer.addPropertyChangeListener(null);
	}

	/** Test method for {@link Player#getCurrentMovement()}. */
	@Test
	public void testGetCurrentMovement() {
		assertEquals("getCurrentMovement() produced an unexpected result!",
				     Movement.DOWN, myPlayer.getCurrentMovement());
	}

	/** Test method for {@link Player#getMovementIndex()}. */
	@Test
	public void testGetMovementIndex() {
		int index = myPlayer.getMovementIndex();
		for (int i = 1; i <= TRIALS; i++) {
			myPlayer.move();
			index = (index + 1) % 4;
			assertEquals("getMovementIndex() produced an unexpected result!",
			             index, myPlayer.getMovementIndex());
		}
	}

	/** Test method for {@link Player#decrementHealth()}. */
	@Test
	public void testDecrementHealth() {
		int hp = Player.MAX_HEALTH;
		for (int i = 1; i <= TRIALS; i++) {
			hp = Math.max(Player.MIN_HEALTH, hp - 1);
			myPlayer.decrementHealth();
			assertEquals("decrementHealth() produced an unexpected result!", 
					     hp, myPlayer.getHealth());
		}
	}

	/** Test method for {@link Player#incrementHealth()}. */
	@Test
	public void testIncrementHealth() {
		for (int i = Player.MIN_HEALTH; i < Player.MAX_HEALTH; i++) {
			myPlayer.decrementHealth();
		}
		int hp = Player.MIN_HEALTH;
		for (int i = 1; i <= TRIALS; i++) {
			hp = Math.min(Player.MAX_HEALTH, hp + 1);
			myPlayer.incrementHealth();
			assertEquals("incrementHealth() produced an unexpected result!", 
					     hp, myPlayer.getHealth());
		}
	}

	/** Test method for {@link Player#getX()}. */
	@Test
	public void testGetX() {
		assertEquals("getX() produced an unexpected result!", 
                     (int) MazeGenerator.getEntryPoint().getX() + Player.CENTER_OFFSET, 
                     myPlayer.getX());
	}

	/** Test method for {@link Player#getY()}. */
	@Test
	public void testGetY() {
		assertEquals("getY() produced an unexpected result!", 
                     (int) MazeGenerator.getEntryPoint().getY() + Player.CENTER_OFFSET, 
                     myPlayer.getY());
	}

	/** Test method for {@link Player#getHealth()}. */
	@Test
	public void testGetHealth() {
		assertEquals("getHealth() produced an unexpected result!", 
				     Player.MAX_HEALTH, myPlayer.getHealth());
	}

	/** Test method for {@link Player#isAdvanceComplete()}. */
	@Test
	public void testIsAdvanceComplete() {
		int distance = 0;
		for (int i = 1; i <= TRIALS; i++) {
			distance += Player.VELOCITY;
			myPlayer.move();
			boolean isComplete = false;
			if (distance == Player.ADVANCE_DISTANCE) {
				isComplete = true;
				distance = 0;
			}
			assertEquals("isAdvanceComplete() produced an unexpected result!",
					     isComplete, myPlayer.isAdvanceComplete());
		}
	}

	/** Test method for {@link Player#setMovement(Movement)}. */
	@Test
	public void testSetMovement() {
		for (final Movement move : Movement.values()) {
			myPlayer.setMovement(move);
			assertEquals("setMovement() produced an unexpected result!",
					     move, myPlayer.getCurrentMovement());
		}
	}
	
	/** Test method for {@link Player#setMovement(Movement)} with a null argument. */
	@Test (expected = NullPointerException.class)
	public void testSetMovementNull() {
		myPlayer.setMovement(null);
	}

	/** Test method for {@link Player#move()} with an upward movement direction. */
	@Test
	public void testMoveUp() {
		myPlayer.setMovement(Movement.UP);
		int currX = myPlayer.getX();
		int currY = myPlayer.getY();
		for (int i = 1; i <= TRIALS; i++) {
			myPlayer.move();
			currY -= Player.VELOCITY;
			assertEquals("move() produced an unexpected result in the upward direction!",
					     new Point(currX, currY), new Point(myPlayer.getX(), myPlayer.getY()));
		}
	}
	
	/** Test method for {@link Player#move()} with a downward movement direction. */
	@Test
	public void testMoveDown() {
		myPlayer.setMovement(Movement.DOWN);
		int currX = myPlayer.getX();
		int currY = myPlayer.getY();
		for (int i = 1; i <= TRIALS; i++) {
			myPlayer.move();
			currY += Player.VELOCITY;
			assertEquals("move() produced an unexpected result in the downward direction!",
					     new Point(currX, currY), new Point(myPlayer.getX(), myPlayer.getY()));
		}
	}
	
	/** Test method for {@link Player#move()} with a leftward movement direction. */
	@Test
	public void testMoveLeft() {
		myPlayer.setMovement(Movement.LEFT);
		int currX = myPlayer.getX();
		int currY = myPlayer.getY();
		for (int i = 1; i <= TRIALS; i++) {
			myPlayer.move();
			currX -= Player.VELOCITY;
			assertEquals("move() produced an unexpected result in the leftward direction!",
					     new Point(currX, currY), new Point(myPlayer.getX(), myPlayer.getY()));
		}
	}
	
	/** Test method for {@link Player#move()} with a rightward movement direction. */
	@Test
	public void testMoveRight() {
		myPlayer.setMovement(Movement.RIGHT);
		int currX = myPlayer.getX();
		int currY = myPlayer.getY();
		for (int i = 1; i <= TRIALS; i++) {
			myPlayer.move();
			currX += Player.VELOCITY;
			assertEquals("move() produced an unexpected result in the rightward direction!",
					     new Point(currX, currY), new Point(myPlayer.getX(), myPlayer.getY()));
		}
	}

}
