package model;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;

public class Player {
	
	private static final int VELOCITY = 8;
	private static final int MOVE_DISTANCE = 48;
	private final Map<Movement, BufferedImage[]> myMovementMap;
	private int myVelX;
	private int myVelY;
	private int myX;
	private int myY;
	private int myDistance;
	private int myMovementIndex;
	private Movement myMovement;
	private BufferedImage mySprite;	
	
	public Player(final int theX, final int theY) {
		myX = theX;
		myY = theY;
		myMovementMap = new HashMap<>();
		fillSprites();
		myMovementIndex = 0;
		myMovement = Movement.DOWN;
		mySprite = myMovementMap.get(myMovement)[myMovementIndex];
		myDistance = 0;
	}
	
	private void fillSprites() {
		try {
			final BufferedImage[] sprites = new BufferedImage[16];
			for (int i = 1; i <= sprites.length; i++) {
				sprites[i-1] = ImageIO.read(new File(String.format("image%d.png", i)));
			}
			
			myMovementMap.put(Movement.DOWN, Arrays.copyOfRange(sprites, 0, 4));
			myMovementMap.put(Movement.UP, Arrays.copyOfRange(sprites, 4, 8));
			myMovementMap.put(Movement.LEFT, Arrays.copyOfRange(sprites, 8, 12));
			myMovementMap.put(Movement.RIGHT, Arrays.copyOfRange(sprites, 12, 16));
			
		} catch (final IOException ex) {
			System.out.println("Could not load sprite image!");
		}
		
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
	
	public boolean isAdvanceComplete() {
		return myDistance == 0;
	}
	
	public int getX() {
		return myX;
	}
	
	public int getY() {
		return myY;
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
		
		setSprite(myMovementMap.get(myMovement)[myMovementIndex]);
	}
}
