package view;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import javax.swing.JPanel;
import javax.swing.Timer;

import controller.KeyboardHandler;
import model.MazeTile;
import model.Movement;
import model.Player;

public class GamePanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2705364087445242453L;
	private static final int WIDTH = 1200;
	private static final int HEIGHT = 800;
	private final MazeTile[] myTiles;
	private Timer myTimer;
	private Player myPlayer;

	public GamePanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		requestFocus();
		myPlayer = new Player(500, 500);
		myTiles = new MazeTile[2];
		addKeyListener(new KeyboardHandler(this));
		myTimer = new Timer(90, theEvent -> advancePlayer());
		setBackground(Color.ORANGE);
		addTiles();
	}
	
	private void addTiles() {
		MazeTile rootTile = new MazeTile(myPlayer.getX() - 5, myPlayer.getY() - 5);
		MazeTile bottTile = new MazeTile(rootTile.getX(), rootTile.getY() + rootTile.getHeight());
		myTiles[0] = rootTile;
		myTiles[1] = bottTile;
	}
	
	@Override
	public void paint(final Graphics theGraphics) {
		super.paint(theGraphics);
		final Graphics2D g2d = (Graphics2D) theGraphics;
		g2d.setColor(Color.BLACK);
		g2d.setStroke(new BasicStroke(6));
		g2d.draw(myTiles[0]);
		g2d.draw(myTiles[1]);
		g2d.setColor(MazeTile.COLOR);
		g2d.fill(myTiles[0]);
		g2d.fill(myTiles[1]);
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
