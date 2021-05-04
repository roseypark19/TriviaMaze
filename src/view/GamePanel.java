package view;

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
import utilities.MazeGenerator;

public class GamePanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2705364087445242453L;
	private static final int WIDTH = 1300;
	private static final int HEIGHT = 865;
	private final Map<Point, MazeTile> myMaze;
	private Timer myTimer;
	private Player myPlayer;

	public GamePanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		requestFocus();
		myMaze = MazeGenerator.getNewMaze();
		myPlayer = new Player(myMaze.get(MazeGenerator.getEntryPoint()));
		addKeyListener(new KeyboardHandler(this));
		myTimer = new Timer(90, theEvent -> advancePlayer());
		setBackground(Color.BLACK);
	}
	
	@Override
	public void paint(final Graphics theGraphics) {
		super.paint(theGraphics);
		final Graphics2D g2d = (Graphics2D) theGraphics;
		for (final MazeTile tile : myMaze.values()) {
			g2d.setColor(MazeTile.COLOR);
			g2d.fill(tile);
		}
		myPlayer.draw(g2d);
	}
	
	public void initializeAdvancement(final Movement theMove) {
		final Point testPoint = myPlayer.getCurrentTile().getPointForMovement(theMove);
		if (!myTimer.isRunning() && myPlayer.isAdvanceComplete() 
				                 && myMaze.containsKey(testPoint)) {
			myPlayer.setMovement(theMove);
			myPlayer.setCurrentTile(myMaze.get(testPoint));
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
