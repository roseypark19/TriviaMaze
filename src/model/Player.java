package model;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Map;
import javax.swing.Timer;

import utilities.MazeGenerator;
import utilities.SpriteUtilities;

public class Player {
	
	private static final Map<Movement, BufferedImage[]> SPRITE_MAP = 
			                                           SpriteUtilities.getPlayerSprites();
	public static final int MIN_HEALTH = 0;
	public static final int MAX_HEALTH = 3;
	public static final String NO_HP = "no hp";
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
	private BufferedImage mySprite;
	
	public Player() {
		final Point entry = MazeGenerator.getEntryPoint();
		myX = (int) (entry.getX() + CENTER_OFFSET);
		myY = (int) (entry.getY() + CENTER_OFFSET);
		myMovementIndex = 0;
		myMovement = Movement.DOWN;
		mySprite = SPRITE_MAP.get(myMovement)[myMovementIndex];
		myDistance = 0;
		myHealth = MAX_HEALTH;
		myNotificationTimer = new Timer(0, theEvent -> notifyNoHealth());
		myNotificationTimer.setRepeats(false);
		myNotificationTimer.setInitialDelay(2500);
		myPcs = new PropertyChangeSupport(this);
	}
	
	public void addPropertyChangeListener(final PropertyChangeListener theListener) {
		myPcs.addPropertyChangeListener(theListener);
	}
	
	public void draw(final Graphics2D theGraphics) {
		theGraphics.drawImage(mySprite, myX, myY, null);
	}
	
	public Movement getCurrentMovement() {
		return myMovement;
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
	}
	
	public void incrementHealth() {
		myHealth = myHealth < MAX_HEALTH ? myHealth + 1 : myHealth;
	}
	
	public int getHealth() {
		return myHealth;
	}
	
	public boolean isAdvanceComplete() {
		return myDistance == 0;
	}
	
	public void setMovement(final Movement theMove) {
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
		mySprite = SPRITE_MAP.get(myMovement)[myMovementIndex];
		update();
	}
	
	public void probeHealthState() {
		if (myHealth == MIN_HEALTH) {
			myNotificationTimer.start();
		}
	}
	
	private void notifyNoHealth() {
		myPcs.firePropertyChange(NO_HP, false, true);
	}
}
