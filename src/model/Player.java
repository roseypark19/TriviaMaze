package model;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.Map;

import utilities.SpriteUtilities;

public class Player {
	
	private static final Map<Movement, BufferedImage[]> SPRITE_MAP = 
			                                           SpriteUtilities.getPlayerSprites();
	public static final int MIN_HEALTH = 0;
	public static final int MAX_HEALTH = 3;
	private static final int VELOCITY = 8;
	private static final int CENTER_OFFSET = 4;
	private static final int MOVE_DISTANCE = 48;
	private int myVelX;
	private int myVelY;
	private int myX;
	private int myY;
	private int myDistance;
	private int myMovementIndex;
	private int myHealth;
	private Movement myMovement;
	private BufferedImage mySprite;
	private MazeTile myMazeTile;
	
	public Player(final MazeTile theTile) {
		myX = (int) (theTile.getX() + CENTER_OFFSET);
		myY = (int) (theTile.getY() + CENTER_OFFSET);
		myMazeTile = theTile;
		myMovementIndex = 0;
		myMovement = Movement.DOWN;
		mySprite = SPRITE_MAP.get(myMovement)[myMovementIndex];
		myDistance = 0;
		myHealth = MAX_HEALTH;
	}
	
	private void setSprite(final BufferedImage theSprite) {
		mySprite = theSprite;
	}
	
	public void draw(final Graphics2D theGraphics) {
		theGraphics.drawImage(mySprite, myX, myY, null);
	}
	
	public void update() {
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
	
	public int getX() {
		return myX;
	}
	
	public int getY() {
		return myY;
	}
	
	public MazeTile getCurrentTile() {
		return myMazeTile;
	}
	
	public void setMovement(final Movement theMove) {
		myMovement = theMove;
	}
	
	public void setCurrentTile(final MazeTile theTile) {
		myMazeTile = theTile;
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
		setSprite(SPRITE_MAP.get(myMovement)[myMovementIndex]);
	}
}
