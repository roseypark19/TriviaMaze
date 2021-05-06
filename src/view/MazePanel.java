package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Timer;
import controller.KeyboardHandler;
import model.MazeTile;
import model.Movement;
import model.Player;
import utilities.MazeGenerator;
import utilities.SpriteUtilities;

public class MazePanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2705364087445242453L;
	private static final BufferedImage GRASS = SpriteUtilities.getGrass();
	private static final Color TRANSPARENT = new Color(0, 0, 0, 0);
	public static final int WIDTH = 1395;
	public static final int HEIGHT = 950;
	private final Map<Point, MazeTile> myMaze;
	private final PlayPanel myPlayPanel;
	private Timer myTimer;
	private Player myPlayer;

	public MazePanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(true);
		myMaze = MazeGenerator.getNewMaze();
		myPlayer = new Player(myMaze.get(MazeGenerator.getEntryPoint()));
		addKeyListener(new KeyboardHandler(this));
		myTimer = new Timer(90, theEvent -> advancePlayer());
		setLayout(new BorderLayout());
//		setBorder(BorderFactory.createLineBorder(Color.YELLOW));
		myPlayPanel = new PlayPanel();
		add(myPlayPanel, BorderLayout.EAST);
		requestFocus();
	}
	
	@Override
	public void paint(final Graphics theGraphics) {
		super.paint(theGraphics);
		final Graphics2D g2d = (Graphics2D) theGraphics;
		g2d.drawImage(GRASS, null, 0, 0);
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
			myPlayPanel.updateKeyButtons();
			myPlayPanel.updateHearts();
		}
	}
	
	private class PlayPanel extends JPanel {
		
		private static final int WIDTH = 530;
		private static final int HEIGHT = MazePanel.HEIGHT;
		private final Set<KeyPadButton> myKeyButtons;
		private final List<JLabel> myHearts;
//		private final JPanel myNorthPanel;
//		private final JPanel myCenterPanel;
		private final JPanel mySouthPanel;
		
		private PlayPanel() {
			setPreferredSize(new Dimension(WIDTH, HEIGHT));
			setBackground(Color.LIGHT_GRAY);
//			setBorder(BorderFactory.createLineBorder(Color.RED));
			setLayout(new BorderLayout());
			myKeyButtons = new HashSet<>();
			myHearts = new ArrayList<>();
			mySouthPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0));
			configureSouthPanel();
			add(mySouthPanel, BorderLayout.SOUTH);
		}
		
		private void configureNorthPanel() {
			// to be added
		}
		
		private void configureCenterPanel() {
			// to be added
		}
		
		private void configureSouthPanel() {
			mySouthPanel.setPreferredSize(new Dimension(WIDTH, 200));
			mySouthPanel.setBackground(TRANSPARENT);
//			mySouthPanel.setBorder(BorderFactory.createLineBorder(Color.BLUE));
			mySouthPanel.add(getKeyPanel());
			updateKeyButtons();
			mySouthPanel.add(Box.createHorizontalStrut(20));
			addHearts();
			updateHearts();
		}
		
		private JPanel getKeyPanel() {
			final JPanel buttonPanel = new JPanel(new GridBagLayout());
			buttonPanel.setBackground(TRANSPARENT);
			final int[] gridXs = new int[] {1, 0, 1, 2};
			final int[] gridYs = new int[] {0, 1, 1, 1};
			final GridBagConstraints gbc = new GridBagConstraints();
			gbc.insets = new Insets(3, 3, 3, 3);
			int index = 0;
			for (final Movement move : Movement.values()) {
				final KeyPadButton b = new KeyPadButton(move);
				myKeyButtons.add(b);
				b.addActionListener(theEvent -> initializeAdvancement(b.getMovement()));
				gbc.gridx = gridXs[index];
				gbc.gridy = gridYs[index];
				buttonPanel.add(b, gbc);
				index++;
			}
//			buttonPanel.setBorder(BorderFactory.createLineBorder(Color.GREEN));
			return buttonPanel;
		}
		
		private void addHearts() {
			for (int count = 1; count <= Player.MAX_HEALTH; count++) {
				final JLabel heart = new JLabel(new ImageIcon("heart.png"));
				heart.setPreferredSize(new Dimension(50, 50));
				myHearts.add(heart);
				mySouthPanel.add(heart);
			}
		}
		
		private void updateKeyButtons() {
			for (final KeyPadButton button : myKeyButtons) {
				final Point testPoint = myPlayer.getCurrentTile().
						                getPointForMovement(button.getMovement());
				button.updateAppearance(myMaze.containsKey(testPoint));
			}
		}
		
		private void updateHearts() {
			final int hp = myPlayer.getHealth();
			for (int count = Player.MAX_HEALTH; count > hp; count--) {
				myHearts.get(count - 1).setEnabled(false);
			}
			for (int count = 1; count <= hp; count++) {
				myHearts.get(count - 1).setEnabled(true);
			}
		}
	}
}
