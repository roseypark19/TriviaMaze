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

import javax.swing.JPanel;
import javax.swing.Timer;

import model.Maze;
import model.Movement;
import model.Player;
import model.SoundType;
import utilities.MazeGenerator;
import utilities.SoundUtilities;
import utilities.SpriteUtilities;

public class MazePanel extends JPanel implements PropertyChangeListener {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2705364087445242453L;
	public static final String INSIDE_TAVERN = "inside tavern";
	public static final String ADVANCE_DONE = "advance done";
	private static final Map<Movement, BufferedImage[]> PLAYER_SPRITE_MAP = 
            									       SpriteUtilities.getPlayerSprites();
	private static final BufferedImage[] TILE_IMAGES = SpriteUtilities.getMazeTiles();
	private static final BufferedImage[] FADES = SpriteUtilities.getFades();
	private static final BufferedImage TAVERN = SpriteUtilities.getTavern();
	private static final BufferedImage FLAGS = SpriteUtilities.getFlags();
	private static final BufferedImage WATER = SpriteUtilities.getWater();
	private static final BufferedImage GRASS = SpriteUtilities.getGrass();
	public static final int WIDTH = 957;
	public static final int HEIGHT = 950;
	private final PropertyChangeSupport myPcs;
	private final Timer myPlayerTimer;
	private final Timer myFadeTimer;
	private final Player myPlayer;
	private final Maze myMaze;
	private int myFadeIndex;
	private boolean myInsideTavern;
	private boolean myFaded;

	public MazePanel(final Player thePlayer, final Maze theMaze) {
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
	
	public void addPropertyChangeListener(final PropertyChangeListener theListener) {
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
	
	public void restoreListeners() {
		myPlayerTimer.addActionListener(theEvent -> advancePlayer());
		myFadeTimer.addActionListener(theEvent -> executeFade());
	}
	
	public void initializeAdvancement(final Movement theMove) {
		final boolean reqs = !myPlayerTimer.isRunning() && !myInsideTavern &&
				             myMaze.isMovementLegal(theMove);     
		if (reqs) {
			myPlayer.setMovement(theMove);
			myMaze.advanceCurrentTile(theMove);
			myPlayerTimer.start();
		}
	}
	
	public void restoreVisibility() {
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
