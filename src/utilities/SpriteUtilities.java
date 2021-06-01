/*
 * SpriteUtilities.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package utilities;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;
import model.Movement;

/**
 * SpriteUtilities is a class which retrieves all sprite images for the Maze Hops game.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class SpriteUtilities {
	
	/**
	 * Retrieves a movement-based mapping to arrays of player sprite images. 
	 * 
	 * @return the movement-based mapping to the individual sprite arrays.
	 */
	public static Map<Movement, BufferedImage[]> getPlayerSprites() {
		final Map<Movement, BufferedImage[]> playerSprites = new HashMap<>();
		try {
			final BufferedImage[] sprites = new BufferedImage[16];
			for (int i = 1; i <= sprites.length; i++) {
				sprites[i-1] = 
				   ImageIO.read(new File(String.format("player_sprites/image%d.png", i)));
			}
			
			playerSprites.put(Movement.DOWN, Arrays.copyOfRange(sprites, 0, 4));
			playerSprites.put(Movement.UP, Arrays.copyOfRange(sprites, 4, 8));
			playerSprites.put(Movement.LEFT, Arrays.copyOfRange(sprites, 8, 12));
			playerSprites.put(Movement.RIGHT, Arrays.copyOfRange(sprites, 12, 16));
			
		} catch (final IOException ex) {
			System.err.println("Could not load sprite image!");
			System.exit(1);
		}
		return playerSprites;
	}
	
	/**
	 * Retrieves the array of sprite images used for maze panel fade ins/outs.
	 * 
	 * @return the array of fade sprite images
	 */
	public static BufferedImage[] getFades() {
		BufferedImage[] fades = new BufferedImage[4];
		try {
			for (int i = 1; i <= fades.length; i++) {
				fades[i - 1] = 
						    ImageIO.read(new File(String.format("fades/%d.png", i * 10)));
			}
		} catch (final IOException ex) {
			System.err.println("Could not load fade image!");
			System.exit(1);
		}
		return fades;
	}
	
	/**
	 * Retrieves the sprite image for the maze panel grass background.
	 * 
	 * @return the grass background sprite image
	 */
	public static BufferedImage getGrass() {
		BufferedImage grass = null;
		try {
			grass = ImageIO.read(new File("maze_sprites/grass.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load grass image!");
			System.exit(1);
		}
		return grass;
	}
	
	/**
	 * Retrieves the sprite image for the play panel background.
	 * 
	 * @return the play panel background sprite image
	 */
	public static BufferedImage getPlayPanelBackground() {
		BufferedImage scroll = null;
		try {
			scroll = ImageIO.read(new File("playpanel_sprites/playPanelBackground.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load play panel background image!");
			System.exit(1);
		}
		return scroll;
	}
	
	/**
	 * Retrieves the sprite image for a tavern.
	 * 
	 * @return the tavern sprite image
	 */
	public static BufferedImage getTavern() {
		BufferedImage tavern = null;
		try {
			tavern = ImageIO.read(new File("maze_sprites/tavern.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load tavern image!");
			System.exit(1);
		}
		return tavern;
	}
	
	/**
	 * Retrieves the sprite image for the finish flags.
	 * 
	 * @return the finish flags sprite image
	 */
	public static BufferedImage getFlags() {
		BufferedImage flag = null;
		try {
			flag = ImageIO.read(new File("maze_sprites/flags.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load tavern image!");
			System.exit(1);
		}
		return flag;
	}
	
	/**
	 * Retrieves the array of sprite images used for maze tiles.
	 * 
	 * @return the array of maze tile sprite images
	 */
	public static BufferedImage[] getMazeTiles() {
		BufferedImage[] tiles = new BufferedImage[5];
		try {
			for (int i = 1; i <= tiles.length; i++) {
				tiles[i - 1] = 
				  ImageIO.read(new File(String.format("maze_sprites/mazetile%d.png", i)));
			}
		} catch (final IOException ex) {
			System.err.println("Could not load tile image!");
			System.exit(1);
		}
		return tiles;
	}
	
	/**
	 * Retrieves the sprite image for a water.
	 * 
	 * @return the water sprite image
	 */
	public static BufferedImage getWater() {
		BufferedImage water = null;
		try {
			water = ImageIO.read(new File("maze_sprites/water.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load water image!");
			System.exit(1);
		}
		return water;
	}
	
	/**
	 * Retrieves the sprite image for the trivia panel background.
	 * 
	 * @return the trivia panel background sprite image
	 */
	public static BufferedImage getTriviaBackground() {
		BufferedImage insideTav = null;
		try {
			insideTav = ImageIO.read(new File("playpanel_sprites/insideTavern.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load trivia panel image!");
			System.exit(1);
		}
		return insideTav;
	}
	
	/**
	 * Retrieves the "how to play" sprite image.
	 * 
	 * @return the how to play sprite image
	 */
	public static BufferedImage getHowToPlayImage() {
		BufferedImage howToPlay = null;
		try {
			howToPlay = ImageIO.read(new File("playpanel_sprites/howToPlay.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load how to play image!");
			System.exit(1);
		}
		return howToPlay;
	}
	
	/**
	 * Retrieves the title screen sprite image.
	 * 
	 * @return the title screen sprite image
	 */
	public static BufferedImage getTitleScreen() {
		BufferedImage title = null;
		try {
			title = ImageIO.read(new File("selectorpanel_sprites/titleScreen.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load title screen image!");
			System.exit(1);
		}
		return title;
	}
	
	/**
	 * Retrieves the game over screen sprite image.
	 * 
	 * @return the game over screen sprite image
	 */
	public static BufferedImage getGameOverScreen() {
		BufferedImage over = null;
		try {
			over = ImageIO.read(new File("selectorpanel_sprites/gameOverScreen.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load game over screen image!");
			System.exit(1);
		}
		return over;
	}

	/**
	 * Retrieves the game won screen sprite image.
	 * 
	 * @return the game won screen sprite image
	 */
	public static BufferedImage getGameWonScreen() {
		BufferedImage won = null;
		try {
			won = ImageIO.read(new File("selectorpanel_sprites/gameWonScreen.png"));
		} catch (final IOException ex) {
			System.err.println("Could not load game won screen image!");
			System.exit(1);
		}
		return won;
	}
}
