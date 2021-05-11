package view;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.Map;
import java.util.Random;

import javax.swing.JPanel;
import javax.swing.Timer;
import controller.KeyboardHandler;
import model.MazeTile;
import model.Movement;
import model.Player;
import model.QuestionType;
import model.Tavern;
import utilities.MazeGenerator;
import utilities.SpriteUtilities;

public class MazePanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2705364087445242453L;
	private static final BufferedImage GRASS = SpriteUtilities.getGrass();
	public static final int WIDTH = 865;
	public static final int HEIGHT = 950;
	private static MazePanel uniqueInstance = new MazePanel();
	private final Map<Point, MazeTile> myMaze;
	private final Map<Point, Tavern> myTaverns;
	private MazeTile myMazeTile;
	private Timer myTimer;

	private MazePanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		myMaze = MazeGenerator.getNewMaze();
		myTaverns = MazeGenerator.getTaverns();
		myMazeTile = myMaze.get(MazeGenerator.getEntryPoint());
		addKeyListener(new KeyboardHandler(this));
		myTimer = new Timer(90, theEvent -> advancePlayer());
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
		for (final MazeTile tile : myMaze.values()) {
			tile.draw(g2d);
		}
		Player.getInstance().draw(g2d);
		for (final Tavern tavern : myTaverns.values()) {
			tavern.draw(g2d);
		}
	}
	
	public void initializeAdvancement(final Movement theMove) {
		final Point testPoint = myMazeTile.getPointForMovement(theMove);
		if (!myTimer.isRunning() && myMaze.containsKey(testPoint)) {
			Player.getInstance().setMovement(theMove);
			myMazeTile = myMaze.get(testPoint);
			myTimer.start();
		}
	}
	
	public boolean isMovementLegal(final Movement theMove) {
		return myMaze.containsKey(myMazeTile.getPointForMovement(theMove));
	}
	
	private void advancePlayer() {
		Player.getInstance().move();
		repaint();
		if (Player.getInstance().isAdvanceComplete()) {
			myTimer.stop();
			Random rand = new Random();
			PlayPanel.getInstance().updateAnswerPanel(QuestionType.values()[rand.nextInt(4)]);
			int random = rand.nextInt(2);
			if (random == 0) {
				Player.getInstance().decrementHealth();
			} else {
				Player.getInstance().incrementHealth();
			}
			PlayPanel.getInstance().updateKeyButtons();
			PlayPanel.getInstance().updateHearts();
			
		}
	}
	
}
