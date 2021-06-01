/*
 * Player.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package model;

import java.awt.Point;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;
import java.util.Objects;

import javax.swing.Timer;

import utilities.MazeGenerator;

/**
 * Player is a class which implements the attributes and behaviors of the Maze Hops player.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class Player implements Serializable {
	
	/** The serial version UID */
	private static final long serialVersionUID = 2488188103432279121L;
	
	/** A player's minimum health */
	public static final int MIN_HEALTH = 0;
	
	/** A player's maximum health */
	public static final int MAX_HEALTH = 3;
	
	/** The bound property indicating that the player has no health remaining */
	public static final String NO_HP = "no hp";
	
	/** The bound property indicating that the player has gained one health */
	public static final String HEALTH_GAINED = "health gained";
	
	/** The bound property indicating that the player has lost one health */
	public static final String HEALTH_LOST = "health lost";
	
	/** A player's velocity */
	private static final int VELOCITY = 8;
	
	/** 
	 * A player's centering offset value - this ensures the player is centered on each
	 * maze tile.
	 */
	private static final int CENTER_OFFSET = 5;
	
	/** The distance a player moves with each advancement */
	private static final int MOVE_DISTANCE = 48;
	
	/** A timer which delays event firings for the NO_HP bound property */
	private final Timer myNotificationTimer;
	
	/** The property change support which fires bound property changes for this player */
	private final PropertyChangeSupport myPcs;
	
	/** This player's horizontal velocity */
	private int myVelX;
	
	/** This player's vertical velocity */
	private int myVelY;
	
	/** This player's horizontal position */
	private int myX;
	
	/** This player's vertical position */
	private int myY;
	
	/** The distance covered by this player */
	private int myDistance;
	
	/** The movement index of this player - used to determine which sprite to display */
	private int myMovementIndex;
	
	/** This player's health */
	private int myHealth;
	
	/** This player's movement direction */
	private Movement myMovement;
	
	/** Constructs a new Player at the maze's default entry point. */
	public Player() {
		final Point entry = MazeGenerator.getEntryPoint();
		myX = (int) (entry.getX() + CENTER_OFFSET);
		myY = (int) (entry.getY() + CENTER_OFFSET);
		myMovementIndex = 0;
		myMovement = Movement.DOWN;
		myDistance = 0;
		myHealth = MAX_HEALTH;
		myNotificationTimer = new Timer(0, theEvent -> notifyNoHealth());
		myNotificationTimer.setRepeats(false);
		myNotificationTimer.setInitialDelay(2700);
		myPcs = new PropertyChangeSupport(this);
	}
	
	/**
	 * Adds a property change listener to this player which listens for changes in bound
	 * properties.
	 * 
	 * @param theListener the property change listener to be assigned
	 * @throws NullPointerException if theListener is null
	 */
	public void addPropertyChangeListener(final PropertyChangeListener theListener) {
		Objects.requireNonNull(theListener, "Property change listeners must be non-null!");
		myPcs.addPropertyChangeListener(theListener);
	}
	
	/** Restores the action listeners associated with this player. */
	public void restoreListeners() {
		myNotificationTimer.addActionListener(theEvent -> notifyNoHealth());
	}
	
	/**
	 * Provides this player's current movement direction.
	 * 
	 * @return the movement direction
	 */
	public Movement getCurrentMovement() {
		return myMovement;
	}
	
	/**
	 * Provides this player's current movement index.
	 * 
	 * @return the movement index
	 */
	public int getMovementIndex() {
		return myMovementIndex;
	}
	
	/**
	 * Updates this player's vertical and horizontal positioning according to its current
	 * vertical and horizontal velocities. This method will also reset the player's 
	 * distance traveled if the advancement distance has been covered.
	 */
	private void update() {
		myX += myVelX;
		myY += myVelY;
		myDistance += Math.max(Math.abs(myVelX), Math.abs(myVelY));
		if (myDistance >= MOVE_DISTANCE) {
			myDistance = 0;
		}
	}
	
	/** Decrements this player's health and notifies all registered listeners. */
	public void decrementHealth() {
		myHealth = myHealth > MIN_HEALTH ? myHealth - 1 : myHealth;
		myPcs.firePropertyChange(HEALTH_LOST, MAX_HEALTH, MIN_HEALTH);
		if (myHealth == MIN_HEALTH) {
			myNotificationTimer.start();
		}
	}
	
	/** Increments this player's health and notifies all registered listeners. */
	public void incrementHealth() {
		myHealth = myHealth < MAX_HEALTH ? myHealth + 1 : myHealth;
		myPcs.firePropertyChange(HEALTH_GAINED, MIN_HEALTH, MAX_HEALTH);
	}
	
	/**
	 * Provides this player's horizontal position.
	 * 
	 * @return the horizontal position
	 */
	public int getX() {
		return myX;
	}
	
	/**
	 * Provides this player's vertical position.
	 * 
	 * @return the vertical position
	 */
	public int getY() {
		return myY;
	}
	
	/**
	 * Provides this player's health.
	 * 
	 * @return the health
	 */
	public int getHealth() {
		return myHealth;
	}
	
	/**
	 * Indicates whether or not this player has completed an advancement.
	 * 
	 * @return true if the advancement is completed, false otherwise
	 */
	public boolean isAdvanceComplete() {
		return myDistance == 0;
	}
	
	/**
	 * Sets this player's movement direction
	 * 
	 * @param theMove the movement direction to be assigned
	 * @throws NullPointerException if theMove is null
	 */
	public void setMovement(final Movement theMove) {
		Objects.requireNonNull(theMove, "Movements must be non-null!");
		myMovement = theMove;
	}
	
	/** 
	 * Updates this player's vertical and horizontal velocities according to its
	 * current movement direction. This method also updates this player's movement index.
	 */
	public void move() {
		myMovementIndex = (myMovementIndex + 1) % 4;
		switch (myMovement) {
			case UP:
				myVelX = 0;
				myVelY = -1 * VELOCITY;
				break;
				
			case DOWN:
				myVelX = 0;
				myVelY = VELOCITY;
				break;
				
			case LEFT:
				myVelX = -1 * VELOCITY;
				myVelY = 0;
				break;
				
			case RIGHT:
				myVelX = VELOCITY;
				myVelY = 0;
				break;
		}
		update();
	}
	
	/** Notifies registered listeners that this player has no health remaining */
	private void notifyNoHealth() {
		myPcs.firePropertyChange(NO_HP, false, true);
	}
}
