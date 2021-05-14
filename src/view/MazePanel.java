package view;

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
import model.QuestionType;
import utilities.SpriteUtilities;

public class MazePanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2705364087445242453L;
	private static final BufferedImage GRASS = SpriteUtilities.getGrass();
	public static final int WIDTH = 957;
	public static final int HEIGHT = 950;
	private static MazePanel uniqueInstance = new MazePanel();
	private Timer myTimer;

	private MazePanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
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
		Maze.getInstance().draw(g2d, false);
		Player.getInstance().draw(g2d);
		Maze.getInstance().draw(g2d, true);
	}
	
	public void initializeAdvancement(final Movement theMove) {
		if (!myTimer.isRunning() && Maze.getInstance().isMovementLegal(theMove)) {
			Player.getInstance().setMovement(theMove);
			Maze.getInstance().advanceCurrentTile(theMove);
			myTimer.start();
		}
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
