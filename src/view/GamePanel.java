package view;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.Timer;

import controller.KeyboardHandler;
import model.MazeTile;
import model.Movement;
import model.Player;
import utilities.MazeFactory;

public class GamePanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2705364087445242453L;
	private static final int WIDTH = 1200;
	private static final int HEIGHT = 820;
	private final Map<Point, MazeTile> myMaze;
//	private final MazeTile[][] myMaze;
	private Timer myTimer;
	private Player myPlayer;

	public GamePanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		requestFocus();
		myMaze = MazeFactory.getMaze();
		myPlayer = new Player((int) myMaze.get(new Point(68, 68)).getX() + 4, (int) myMaze.get(new Point(68, 68)).getY() + 4);
		addKeyListener(new KeyboardHandler(this));
		myTimer = new Timer(90, theEvent -> advancePlayer());
		setBackground(Color.ORANGE);
	}
	
	@Override
	public void paint(final Graphics theGraphics) {
		super.paint(theGraphics);
		final Graphics2D g2d = (Graphics2D) theGraphics;
		for (final MazeTile tile : myMaze.values()) {
			g2d.setColor(Color.BLACK);
			g2d.setStroke(new BasicStroke(10));
			g2d.draw(tile);
			g2d.setColor(MazeTile.COLOR);
			g2d.fill(tile);
		}
//		for (int row = 0; row < myMaze.length; row++) {
//			for (int col = 0; col < myMaze.length; col++) {
//				g2d.setColor(Color.BLACK);
//				g2d.setStroke(new BasicStroke(6));
//				g2d.draw(myMaze[row][col]);
//				g2d.setColor(MazeTile.COLOR);
//				g2d.fill(myMaze[row][col]);
//			}
//		}
		myPlayer.draw(g2d);
	}
	
	public void initializeAdvancement(final Movement theMove) {
		if (!myTimer.isRunning() && myPlayer.isAdvanceComplete()) {
			myPlayer.setMovement(theMove);
			myTimer.start();
		}
	}
	
	private void advancePlayer() {
		myPlayer.move();
		myPlayer.update();
		repaint();
		if (myPlayer.isAdvanceComplete()) {
			myTimer.stop();
		}
	}
}
