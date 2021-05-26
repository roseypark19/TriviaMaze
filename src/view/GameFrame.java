package view;

import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSlider;

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
	private static final ImageIcon NEW_GRASS = new ImageIcon("newGame.png");
	private static final ImageIcon LOAD_GRASS = new ImageIcon("loadGame.png");
	private static final ImageIcon NEW_SAND = new ImageIcon("newGame_Over.png");
	private static final ImageIcon LOAD_SAND = new ImageIcon("loadGame_Over.png");
	private static final int MAX_VOLUME = 100;
	private static final int DEFAULT_VOLUME = 50;

	public GameFrame() {
		setLoopingMusic(SoundType.TITLE);
		setTitle("Maze Hops");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		JMenuBar myMenu = new JMenuBar();
		setupMenuBar(myMenu);
		setJMenuBar(myMenu);
		mySelectorPanel = new SelectorPanel();
		myCurrentPanel = mySelectorPanel;
		add(myCurrentPanel);
		myCurrentPanel.grabFocus();
		pack();
		setLocationRelativeTo(null);
		setResizable(false);
		setVisible(true);
	}

	private void setupMenuBar(final JMenuBar theMenu) {
		JMenu fileMenu = new JMenu("File");
		theMenu.add(fileMenu);
		
		JMenu aboutMenu = new JMenu("About");
		theMenu.add(aboutMenu);
		
		JMenu optionsMenu = new JMenu("Options");
		theMenu.add(optionsMenu);
		
		JMenu helpMenu = new JMenu("Help");
		theMenu.add(helpMenu);
		
		/* File Menu */
		JMenuItem newGame = new JMenuItem("New Game");
		JMenuItem saveGame = new JMenuItem("Save Game");
		JMenuItem loadGame = new JMenuItem("Load Game");
		JMenuItem exitGame = new JMenuItem("Exit");
		fileMenu.add(newGame);
		fileMenu.add(saveGame);
		fileMenu.add(loadGame);
		fileMenu.addSeparator();
		fileMenu.add(exitGame);
		newGame.addActionListener(theEvent -> newGame());
		saveGame.addActionListener(theEvent -> saveGame());
		loadGame.addActionListener(theEvent -> loadGame());
		exitGame.addActionListener(theEvent -> System.exit(0));
		
		/* About Menu */
		JMenuItem aboutGame = new JMenuItem("About");
		aboutMenu.add(aboutGame);
		aboutGame.addActionListener(theEvent -> aboutMenu());
		
		/* Options Menu */
		JLabel volumeLabel = new JLabel("Adjust Volume");
		JSlider volumeSlider = new JSlider(0,MAX_VOLUME,DEFAULT_VOLUME);
		volumeSlider.setPaintLabels(rootPaneCheckingEnabled);
		volumeSlider.setPaintTicks(true);
		volumeSlider.setPaintTrack(true);
		volumeSlider.setMajorTickSpacing(25);
		volumeSlider.setMinorTickSpacing(5);
		optionsMenu.add(volumeLabel);
		optionsMenu.add(volumeSlider);

		/* Help Menu */
		JMenuItem helpItem = new JMenuItem("Help");
		helpMenu.add(helpItem);
	}
	
	private void aboutMenu() {
		
	}

	@Override
	public void propertyChange(final PropertyChangeEvent theEvent) {
		if (theEvent.getPropertyName().equals(Player.NO_HP)) {
			displaySelectorPanel(false);
			// play lose sound
		} else if (theEvent.getPropertyName().equals(Maze.END_REACHED)) {
			displaySelectorPanel(true);
			SoundUtilities.play(SoundType.WIN);
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
		myCurrentPanel.grabFocus();
		setLoopingMusic(SoundType.BACKGROUND);
	}
	
	private void displaySelectorPanel(final boolean theWon) {
		remove(myCurrentPanel);
		mySelectorPanel.setButtonIcons(theWon);
		mySelectorPanel.setImage(theWon);
		myCurrentPanel = mySelectorPanel;
		add(myCurrentPanel);
		mySelectorPanel.repaint();
		revalidate();
		myCurrentPanel.grabFocus();
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
		JOptionPane.showConfirmDialog(this, message, "Error Saving Game!", 
				                                     JOptionPane.ERROR_MESSAGE);
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
				remove(myCurrentPanel);
				myGamePanel = loadedGame;
				if (myGamePanel.isGameOver()) {
					displaySelectorPanel(false);
				} else if (myGamePanel.isGameWon()) {
					displaySelectorPanel(true);
				} else {
					myGamePanel.addPropertyChangeListener(Player.NO_HP, this);
					myGamePanel.addPropertyChangeListener(Maze.END_REACHED, this);
					myCurrentPanel = myGamePanel;
					add(myCurrentPanel);
					revalidate();
					myCurrentPanel.grabFocus();
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
				                                     JOptionPane.ERROR_MESSAGE);
	}
	
	private class SelectorPanel extends JPanel {
		
		/**
		 * 
		 */
		private static final long serialVersionUID = 2071944460810159409L;
		private final JButton myNewButton;
		private final JButton myLoadButton;
		private BufferedImage myImage;
		
		private SelectorPanel() {
			myImage = TITLE;
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
			g2d.drawImage(myImage, null, 0, 0);
		}
		
		private void setImage(final boolean theWon) {
			myImage = theWon ? GAME_WON : GAME_OVER;
		}
		
		private void setButtonIcons(final boolean theWon) {
			final ImageIcon newIcon = theWon ? NEW_GRASS : NEW_SAND;
			final ImageIcon loadIcon = theWon ? LOAD_GRASS : LOAD_SAND;
			myNewButton.setIcon(newIcon);
			myLoadButton.setIcon(loadIcon);
		}
	}

	

}
