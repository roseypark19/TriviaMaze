package model;

import java.awt.Point;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;
import java.util.Objects;

import javax.swing.Timer;

import utilities.MazeGenerator;

public class Player implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2488188103432279121L;
	public static final int MIN_HEALTH = 0;
	public static final int MAX_HEALTH = 3;
	public static final String NO_HP = "no hp";
	public static final String HEALTH_GAINED = "health gained";
	public static final String HEALTH_LOST = "health lost";
	private static final int VELOCITY = 8;
	private static final int CENTER_OFFSET = 5;
	private static final int MOVE_DISTANCE = 48;
	private final Timer myNotificationTimer;
	private final PropertyChangeSupport myPcs;
	private int myVelX;
	private int myVelY;
	private int myX;
	private int myY;
	private int myDistance;
	private int myMovementIndex;
	private int myHealth;
	private Movement myMovement;
	
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
	
	public void addPropertyChangeListener(final PropertyChangeListener theListener) {
		Objects.requireNonNull(theListener, "Property change listeners must be non-null!");
		myPcs.addPropertyChangeListener(theListener);
	}
	
	public void restoreListeners() {
		myNotificationTimer.addActionListener(theEvent -> notifyNoHealth());
	}
	
	public Movement getCurrentMovement() {
		return myMovement;
	}
	
	public int getMovementIndex() {
		return myMovementIndex;
	}
	
	private void update() {
		myX += myVelX;
		myY += myVelY;
		myDistance += Math.max(Math.abs(myVelX), Math.abs(myVelY));
		if (myDistance >= MOVE_DISTANCE) {
			myDistance = 0;
		}
	}
	
	public void decrementHealth() {
		myHealth = myHealth > MIN_HEALTH ? myHealth - 1 : myHealth;
		myPcs.firePropertyChange(HEALTH_LOST, MAX_HEALTH, MIN_HEALTH);
		if (myHealth == MIN_HEALTH) {
			myNotificationTimer.start();
		}
	}
	
	public void incrementHealth() {
		myHealth = myHealth < MAX_HEALTH ? myHealth + 1 : myHealth;
		myPcs.firePropertyChange(HEALTH_GAINED, MIN_HEALTH, MAX_HEALTH);
	}
	
	public int getX() {
		return myX;
	}
	
	public int getY() {
		return myY;
	}
	
	public int getHealth() {
		return myHealth;
	}
	
	public boolean isAdvanceComplete() {
		return myDistance == 0;
	}
	
	public void setMovement(final Movement theMove) {
		Objects.requireNonNull(theMove, "Movements must be non-null!");
		myMovement = theMove;
	}
	
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
	
	private void notifyNoHealth() {
		myPcs.firePropertyChange(NO_HP, false, true);
	}
}
