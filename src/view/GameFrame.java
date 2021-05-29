package view;

import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import model.Maze;
import model.Player;
import model.SoundType;
import utilities.SoundUtilities;
import utilities.SpriteUtilities;

public class GameFrame extends JFrame implements PropertyChangeListener {
	
	private final SelectorPanel mySelectorPanel;
	private GamePanel myGamePanel;
	private JPanel myCurrentPanel;

	/**
	 * 
	 */
	private static final long serialVersionUID = -997412424190795317L;
	private static final BufferedImage TITLE = SpriteUtilities.getTitleScreen();
	private static final BufferedImage GAME_OVER = SpriteUtilities.getGameOverScreen();
	private static final BufferedImage GAME_WON = SpriteUtilities.getGameWonScreen();
	private static final ImageIcon ERROR_ICON = new ImageIcon("playpanel_sprites/redX.png");
	private static final ImageIcon NEW_GRASS = new ImageIcon("selectorpanel_sprites/newGame.png");
	private static final ImageIcon LOAD_GRASS = new ImageIcon("selectorpanel_sprites/loadGame.png");
	private static final ImageIcon NEW_SAND = new ImageIcon("selectorpanel_sprites/newGame_Over.png");
	private static final ImageIcon LOAD_SAND = new ImageIcon("selectorpanel_sprites/loadGame_Over.png");

	public GameFrame() {
		setLoopingMusic(SoundType.TITLE);
		setTitle("Maze Hops");
		final ImageIcon beerIcon = new ImageIcon("frame_icon/beerIcon.png");
		final Image image = beerIcon.getImage().getScaledInstance(15, -1, Image.SCALE_SMOOTH);
		setIconImage(image);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		mySelectorPanel = new SelectorPanel();
		myCurrentPanel = mySelectorPanel;
		add(myCurrentPanel);
		myCurrentPanel.grabFocus();
		pack();
		setLocationRelativeTo(null);
		setResizable(false);
		setVisible(true);
	}

	@Override
	public void propertyChange(final PropertyChangeEvent theEvent) {
		if (theEvent.getPropertyName().equals(Player.NO_HP)) {
			displaySelectorPanel();
		} else if (theEvent.getPropertyName().equals(Maze.END_REACHED)) {
			displaySelectorPanel();
		}
	}
	
	private void setLoopingMusic(final SoundType theType) {
		if (SoundUtilities.isPlaying(SoundType.TITLE)) {
			SoundUtilities.stop(SoundType.TITLE);
			SoundUtilities.play(theType);
		} else if (SoundUtilities.isPlaying(SoundType.BACKGROUND)) {
			SoundUtilities.stop(SoundType.BACKGROUND);
			SoundUtilities.play(theType);
		} else {
			SoundUtilities.play(theType);
		}
	}
	
	private void newGame() {
		remove(myCurrentPanel);
		myGamePanel = new GamePanel();
		myGamePanel.addPropertyChangeListener(Player.NO_HP, this);
		myGamePanel.addPropertyChangeListener(Maze.END_REACHED, this);
		myCurrentPanel = myGamePanel;
		add(myCurrentPanel);
		revalidate();
		myCurrentPanel.requestFocus();
		setLoopingMusic(SoundType.BACKGROUND);
	}
	
	private void displaySelectorPanel() {
		remove(myCurrentPanel);
		mySelectorPanel.setButtonIcons();
		myCurrentPanel = mySelectorPanel;
		add(myCurrentPanel);
		mySelectorPanel.repaint();
		revalidate();
		myCurrentPanel.requestFocus();
		if (myGamePanel.isGameWon()) {
			SoundUtilities.play(SoundType.WIN);
		} else {
			SoundUtilities.play(SoundType.LOSE);
		}
		setLoopingMusic(SoundType.TITLE);
	}
	
	private void saveGame() {
		final FileDialog fd = new FileDialog(this, "Save Game", FileDialog.SAVE);
		fd.setVisible(true);
		if (fd.getFile() != null) {
			final String fileName = fd.getFile(); // possible file extension?
			try {
				final File f = new File(fd.getDirectory(), fileName);
				final FileOutputStream file = new FileOutputStream(f);
				final ObjectOutputStream out = new ObjectOutputStream(file);
				out.writeObject(myGamePanel);
				out.close();
				file.close();
			} catch (IOException ex) {
				showSaveError(ex.getClass().getSimpleName(), fileName);
			}
		}
	}
	
	private void showSaveError(final String theExceptionName, final String theFileName) {
		final String message = String.format("Could not save game: \"%s\"\nSource: %s", 
				                             theFileName, theExceptionName);
		JOptionPane.showMessageDialog(this, message, "Error Saving Game!", 
                                      JOptionPane.ERROR_MESSAGE, ERROR_ICON);
	}
	
	private void loadGame() {
		final FileDialog fd = new FileDialog(this, "Load Game", FileDialog.LOAD);
		fd.setVisible(true);
		if (fd.getFile() != null) {
			try {
				final File f = new File(fd.getDirectory(), fd.getFile());
				final FileInputStream file = new FileInputStream(f);
				final ObjectInputStream in = new ObjectInputStream(file);
				final GamePanel loadedGame = (GamePanel) in.readObject();
				myGamePanel = loadedGame;
				if (myGamePanel.isGameOver() || myGamePanel.isGameWon()) {
					displaySelectorPanel();
				} else {
					remove(myCurrentPanel);
					myGamePanel.addPropertyChangeListener(this);
					myGamePanel.restoreListeners();
					myCurrentPanel = myGamePanel;
					add(myCurrentPanel);
					revalidate();
					setLoopingMusic(SoundType.BACKGROUND);
					myCurrentPanel.requestFocus();
				}
				in.close();
				file.close();
			} catch (Exception ex) {
				showLoadError(ex.getClass().getSimpleName(), fd.getFile());
			}
		}
	}
	
	private void showLoadError(final String theExceptionName, final String theFileName) {
		final String message = String.format("Could not load game: \"%s\"\nSource: %s", 
                							 theFileName, theExceptionName);
		JOptionPane.showMessageDialog(this, message, "Error Loading Game!", 
				                      JOptionPane.ERROR_MESSAGE, ERROR_ICON);
	}
	
	private class SelectorPanel extends JPanel {
		
		/**
		 * 
		 */
		private static final long serialVersionUID = 2071944460810159409L;
		private final JButton myNewButton;
		private final JButton myLoadButton;
		
		private SelectorPanel() {
			setPreferredSize(new Dimension(MazePanel.WIDTH + PlayPanel.WIDTH,
										   MazePanel.HEIGHT));
			setLayout(new FlowLayout(FlowLayout.CENTER, 75, 0));
			add(Box.createRigidArea(new Dimension(1487, 620)));
			myNewButton = new JButton(NEW_GRASS);
			myNewButton.addActionListener(theEvent -> newGame());
			myNewButton.setPreferredSize(new Dimension(367, 82));
			myNewButton.setFocusable(false);
			myNewButton.setBorderPainted(false);
			add(myNewButton);
			myLoadButton = new JButton(LOAD_GRASS);
			myLoadButton.addActionListener(theEvent -> loadGame());
			myLoadButton.setPreferredSize(new Dimension(367, 82));
			myLoadButton.setFocusable(false);
			myLoadButton.setBorderPainted(false);
			add(myLoadButton);
		}
		
		@Override
		public void paintComponent(final Graphics theGraphics) {
			super.paintComponent(theGraphics);
			final Graphics2D g2d = (Graphics2D) theGraphics;
			BufferedImage image = null;
			if (myGamePanel != null && myGamePanel.isGameOver()) {
				image = GAME_OVER;
			} else if (myGamePanel != null && myGamePanel.isGameWon()) {
				image = GAME_WON;
			} else {
				image = TITLE;
			}
			g2d.drawImage(image, null, 0, 0);
		}
		
		private void setButtonIcons() {
			final ImageIcon newIcon = myGamePanel.isGameWon() ? NEW_GRASS : NEW_SAND;
			final ImageIcon loadIcon = myGamePanel.isGameWon() ? LOAD_GRASS : LOAD_SAND;
			myNewButton.setIcon(newIcon);
			myLoadButton.setIcon(loadIcon);
		}
	}

}
