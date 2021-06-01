/*
 * MazePanel.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Map;
import java.util.Objects;

import javax.swing.JPanel;
import javax.swing.Timer;

import model.Maze;
import model.Movement;
import model.Player;
import model.SoundType;
import utilities.MazeGenerator;
import utilities.SoundUtilities;
import utilities.SpriteUtilities;

/**
 * MazePanel is a class for displaying the player and maze contents for the Maze Hops game.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class MazePanel extends JPanel implements PropertyChangeListener {
	
	/** The serial version UID */
	private static final long serialVersionUID = 2705364087445242453L;
	
	/** The bound property indicating that the player has entered a tavern */
	public static final String INSIDE_TAVERN = "inside tavern";
	
	/** The bound property indicating that a player advancement animation is complete */
	public static final String ADVANCE_DONE = "advance done";
	
	/** The mapping of movement directions to arrays of player sprite images */
	private static final Map<Movement, BufferedImage[]> PLAYER_SPRITE_MAP = 
            									       SpriteUtilities.getPlayerSprites();
	
	/** The array of maze tile sprite images */
	private static final BufferedImage[] TILE_IMAGES = SpriteUtilities.getMazeTiles();
	
	/** The array of fade in/out sprite images */
	private static final BufferedImage[] FADES = SpriteUtilities.getFades();
	
	/** The tavern sprite image */
	private static final BufferedImage TAVERN = SpriteUtilities.getTavern();
	
	/** The finish flags sprite image */
	private static final BufferedImage FLAGS = SpriteUtilities.getFlags();
	
	/** The water sprite image */
	private static final BufferedImage WATER = SpriteUtilities.getWater();
	
	/** The grass background sprite image */
	private static final BufferedImage GRASS = SpriteUtilities.getGrass();
	
	/** A maze panel's width */
	public static final int WIDTH = 957;
	
	/** A maze panel's height */
	public static final int HEIGHT = 950;
	
	/** The property change support which fires bound property changes for this maze panel */
	private final PropertyChangeSupport myPcs;
	
	/** The timer which controls player movement animation timing */
	private final Timer myPlayerTimer;
	
	/** The timer which controls screen fade timing */
	private final Timer myFadeTimer;
	
	/** This maze panel's player */
	private final Player myPlayer;
	
	/** This maze panel's maze */
	private final Maze myMaze;
	
	/** This maze panel's fade index for determining which fade sprite image to display */
	private int myFadeIndex;
	
	/** Indicates whether or not this maze panel's player is currently inside a tavern */
	private boolean myInsideTavern;
	
	/** Indicates whether or not this maze panel's display is currently faded out */
	private boolean myFaded;

	/**
	 * Constructs a new MazePanel, assigning the specified player and maze.
	 * 
	 * @param thePlayer the player to be assigned
	 * @param theMaze the maze to be assigned
	 * @throws NullPointerException if thePlayer or theMaze is null
	 */
	public MazePanel(final Player thePlayer, final Maze theMaze) {
		Objects.requireNonNull(thePlayer, "Players must be non-null!");
		Objects.requireNonNull(theMaze, "Mazes must be non-null!");
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		setBackground(Color.BLACK);
		myPlayer = thePlayer;
		myMaze = theMaze;
		myPcs = new PropertyChangeSupport(this);
		myPlayerTimer = new Timer(90, theEvent -> advancePlayer());
		myFadeTimer = new Timer(75, theEvent -> executeFade()); 
		myInsideTavern = false;
		myFaded = false;
		myFadeIndex = 0;
		setFocusable(false);
	}
	
	/**
	 * Adds a property change listener to this maze panel which listens for changes in bound
	 * properties.
	 * 
	 * @param theListener the property change listener to be assigned
	 * @throws NullPointerException if theListener is null
	 */
	public void addPropertyChangeListener(final PropertyChangeListener theListener) {
		Objects.requireNonNull(theListener, "Property change listeners must be non-null!");
		myPcs.addPropertyChangeListener(theListener);
	}
	
	@Override
	public void propertyChange(final PropertyChangeEvent theEvent) {
		if (theEvent.getPropertyName().equals(PlayPanel.TRIVIA_ANSWERED)) {
			myInsideTavern = false;
			restoreVisibility();
		}
	}
	
	@Override
	public void paintComponent(final Graphics theGraphics) {
		super.paintComponent(theGraphics);
		final Graphics2D g2d = (Graphics2D) theGraphics;
		g2d.drawImage(GRASS, null, 0, 0);
		for (final Map.Entry<Point, Integer> entry : myMaze.getTileData().entrySet()) {
			final Point pt = entry.getKey();
			g2d.drawImage(TILE_IMAGES[entry.getValue()], null, (int) pt.getX(), (int) pt.getY());
		}
		g2d.drawImage(FLAGS, (int) MazeGenerator.getExitPoint().getX() + 1, 
                             (int) MazeGenerator.getExitPoint().getY(), null);
		for (final Point pt : myMaze.getWaterPoints()) {
			g2d.drawImage(WATER, (int) pt.getX() + 12, (int) pt.getY() + 6, null);
		}
		g2d.drawImage(PLAYER_SPRITE_MAP.get(myPlayer.getCurrentMovement())[myPlayer.getMovementIndex()], 
				      null, myPlayer.getX(), myPlayer.getY());
		for (final Point pt : myMaze.getTavernPoints()) {
			g2d.drawImage(TAVERN, null, (int) pt.getX(), (int) pt.getY());
		}
		if (myFadeTimer.isRunning()) {
			for (int index = 0; index <= myFadeIndex; index++) {
				g2d.drawImage(FADES[index], null, 0, 0);
			}
		}
	}
	
	/** Restores the action listeners associated with this maze panel. */
	public void restoreListeners() {
		myMaze.restoreListeners();
		myPlayerTimer.addActionListener(theEvent -> advancePlayer());
		myFadeTimer.addActionListener(theEvent -> executeFade());
	}
	
	/**
	 * Initializes a single maze tile player advancement animation in the provided movement
	 * direction. Note that this method will only follow through with the advancement
	 * if an advancement is not currently taking place, the player is not inside a tavern, and
	 * the desired movement direction results in a legal tile position on this maze panel's maze.
	 * 
	 * @param theMove the desired movement direction
	 * @throws NullPointerException if theMove is null
	 */
	public void initializeAdvancement(final Movement theMove) {
		Objects.requireNonNull(theMove, "Movements must be non-null!");
		final boolean reqs = !myPlayerTimer.isRunning() && !myInsideTavern &&
				             myMaze.isMovementLegal(theMove);     
		if (reqs) {
			myPlayer.setMovement(theMove);
			myMaze.advanceCurrentTile(theMove);
			myPlayerTimer.start();
		}
	}
	
	/** 
	 * Removes the fade out effect from this maze panel, removing any correctly answered taverns
	 * or kicking the player out of an unanswered tavern.
	 */
	private void restoreVisibility() {
		if (!myMaze.getTavernTrivia().isAnswered()) {
			myPlayerTimer.setInitialDelay(800);
			initializeAdvancement(myPlayer.getCurrentMovement().getOpposite());
		} else {
			myMaze.removeTavern();
			myPcs.firePropertyChange(ADVANCE_DONE, false, true);
		}
		myFadeTimer.start();
		myFadeTimer.setInitialDelay(0);
		myPlayerTimer.setInitialDelay(0);
	}
	
	/** 
	 * Advances this maze panel's player by a single move and performs checks for taverns or
	 * waters if the player has completed the current advancement.
	 */
	private void advancePlayer() {
		myPlayer.move();
		repaint();
		if (myPlayer.isAdvanceComplete()) {
			myPlayerTimer.stop();
			if (myMaze.hasTavern()) {
				myInsideTavern = true;
				myFadeTimer.start();
				myPcs.firePropertyChange(INSIDE_TAVERN, false, true);
			} else if (myMaze.hasWater()) {
				SoundUtilities.play(SoundType.DRINK);
				myPlayer.incrementHealth();
				myMaze.removeWater();
			}
			myPcs.firePropertyChange(ADVANCE_DONE, false, true);
		}
	}
	
	/**
	 * Performs a fade in/out on this maze panel to transition between answering tavern trivia.
	 */
	private void executeFade() {
		final Graphics2D g2d = (Graphics2D) getGraphics();
		if (myFaded) {
			repaint();
		} else {
			g2d.drawImage(FADES[myFadeIndex], null, 0, 0);
		}
		myFadeIndex = myFaded ? myFadeIndex - 1 : myFadeIndex + 1;
		if (myFadeIndex < 0 || myFadeIndex > FADES.length - 1) {
			myFadeTimer.stop();
			myFaded = !myFaded;
			myFadeIndex = myFaded ? FADES.length - 1 : 0;
		}
	}

}
