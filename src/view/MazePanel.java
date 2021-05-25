package view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import javax.swing.JPanel;
import javax.swing.Timer;

import model.Maze;
import model.Movement;
import model.Player;
import model.SoundType;
import model.Trivia;
import utilities.SoundUtilities;
import utilities.SpriteUtilities;

public class MazePanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2705364087445242453L;
	private static final BufferedImage[] FADES = SpriteUtilities.getFades();
	private static final BufferedImage GRASS = SpriteUtilities.getGrass();
	public static final int WIDTH = 957;
	public static final int HEIGHT = 950;
	private final Timer myPlayerTimer;
	private final Timer myFadeTimer;
	private final Player myPlayer;
	private final Maze myMaze;
	private PlayPanel myPlayPanel;
	private TriviaPanel myTriviaPanel;
	private int myFadeIndex;
	private boolean myFaded;

	public MazePanel(final Player thePlayer, final Maze theMaze) {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		setBackground(Color.BLACK);
		addKeyListener(new KeyboardListener());
		myPlayer = thePlayer;
		myMaze = theMaze;
		myPlayerTimer = new Timer(90, theEvent -> advancePlayer());
		myFadeTimer = new Timer(75, theEvent -> executeFade()); 
		myFaded = false;
		myFadeIndex = 0;
		setFocusable(true);
		requestFocus();
	}
	
	public void connectPanels(final PlayPanel thePlayPan, final TriviaPanel theTrivPan) {
		if (myPlayPanel != null || myTriviaPanel != null) {
			throw new IllegalStateException("Panel connections have already been made!");
		}
		myPlayPanel = thePlayPan;
		myTriviaPanel = theTrivPan;
	}
	
	@Override
	public void paintComponent(final Graphics theGraphics) {
		super.paintComponent(theGraphics);
		final Graphics2D g2d = (Graphics2D) theGraphics;
		g2d.drawImage(GRASS, null, 0, 0);
		myMaze.draw(g2d, false);
		myPlayer.draw(g2d);
		myMaze.draw(g2d, true);
		if (myFadeTimer.isRunning()) {
			for (int index = 0; index <= myFadeIndex; index++) {
				g2d.drawImage(FADES[index], null, 0, 0);
			}
		}
	}
	
	public void initializeAdvancement(final Movement theMove) {
		final boolean reqs = !myPlayerTimer.isRunning() && 
				             myMaze.isMovementLegal(theMove) && 
				             !myTriviaPanel.isAsking();
		if (reqs) {
			myPlayer.setMovement(theMove);
			myMaze.advanceCurrentTile(theMove);
			myPlayerTimer.start();
		}
	}
	
	public void restoreVisibility(final boolean theAnsweredCorrectly) {
		myFadeTimer.start();
		if (!theAnsweredCorrectly) {
			myPlayerTimer.setInitialDelay(800);
			initializeAdvancement(myPlayer.getCurrentMovement().getOpposite());
		}
		myFadeTimer.setInitialDelay(0);
		myPlayerTimer.setInitialDelay(0);
	}
	
	private void advancePlayer() {
		myPlayer.move();
		repaint();
		if (myPlayer.isAdvanceComplete()) {
			myPlayerTimer.stop();
			myPlayPanel.updateKeyButtons();
			if (myMaze.hasTavern()) {
				final Trivia triv = myMaze.getTavernTrivia();
				myFadeTimer.start();
				myTriviaPanel.setupNewTrivia(triv);
			} else if (myMaze.hasWater()) {
				SoundUtilities.play(SoundType.DRINK);
				myPlayer.incrementHealth();
				myPlayPanel.updateHearts();
				myMaze.removeWater();
			}
			myMaze.checkEndReached();
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
	
	private class KeyboardListener extends KeyAdapter {
		
		private boolean myReleased;
		
		public KeyboardListener() {
			myReleased = true;
		}

		@Override
		public void keyTyped(final KeyEvent theEvent) {
			if (myReleased) {
				initializeAdvancement(Movement.valueof(Character.toUpperCase(
													   theEvent.getKeyChar())));
				myReleased = false;
			}	
		}
		
		@Override
		public void keyReleased(final KeyEvent theEvent) {
			myReleased = true;
		}
	}
	
}
