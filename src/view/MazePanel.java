package view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.Random;

import javax.swing.JPanel;
import javax.swing.Timer;
import controller.KeyboardHandler;
import model.Maze;
import model.Movement;
import model.Player;
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
	private static MazePanel uniqueInstance = new MazePanel();
	private final Timer myPlayerTimer;
	private final Timer myFadeTimer;
	private int myFadeIndex;
	private boolean myFaded;

	private MazePanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		setBackground(Color.BLACK);
		addKeyListener(new KeyboardHandler(this));
		myPlayerTimer = new Timer(90, theEvent -> advancePlayer());
		myFadeTimer = new Timer(150, theEvent -> executeFade()); 
		myFaded = false;
		myFadeIndex = 0;
		requestFocus();
	}
	
	public static synchronized MazePanel getInstance() {
		return uniqueInstance;
	}
	
	@Override
	public void paintComponent(final Graphics theGraphics) {
		super.paintComponent(theGraphics);
		final Graphics2D g2d = (Graphics2D) theGraphics;
		g2d.drawImage(GRASS, null, 0, 0);
		Maze.getInstance().draw(g2d, false);
		Player.getInstance().draw(g2d);
		Maze.getInstance().draw(g2d, true);
		if (myFadeTimer.isRunning()) {
			for (int index = 0; index <= myFadeIndex; index++) {
				g2d.drawImage(FADES[index], null, 0, 0);
			}
		}
	}
	
	public void initializeAdvancement(final Movement theMove) {
		if (!myPlayerTimer.isRunning() && Maze.getInstance().isMovementLegal(theMove)) {
			Player.getInstance().setMovement(theMove);
			Maze.getInstance().advanceCurrentTile(theMove);
			myPlayerTimer.start();
		}
	}
	
	private void advancePlayer() {
		Player.getInstance().move();
		repaint();
		if (Player.getInstance().isAdvanceComplete()) {
			myPlayerTimer.stop();
			// THESE FUNCTIONALITIES ARE ONLY HERE FOR TESTING AND WILL NOT BE IMPLEMENTED HERE!
			// UNCOMMENT THESE TO TEST OUT FADE FEATURES AND HEART BEATS.
//			if (Maze.getInstance().hasTavern()) {
//				myFadeTimer.start();
//			}
//			Random rand = new Random();
//			int random = rand.nextInt(2);
//			if (random == 0) {
//				PlayPanel.getInstance().initializeHeartBeat();
//				Player.getInstance().decrementHealth();
//			} else {
//				Player.getInstance().incrementHealth();
//				PlayPanel.getInstance().updateHearts();
//			}
//			PlayPanel.getInstance().updateKeyButtons();
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
