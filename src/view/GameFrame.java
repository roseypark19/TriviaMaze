/*
 * GameFrame.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package view;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
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

/**
 * GameFrame is a class which contains the main game panel as well as a selector panel
 * for selecting to start/load a game.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class GameFrame extends JFrame implements PropertyChangeListener {

	/** The serial version UID */
	private static final long serialVersionUID = -997412424190795317L;
	
	/** The title screen sprite image */
	private static final BufferedImage TITLE = SpriteUtilities.getTitleScreen();
	
	/** The game over screen sprite image */
	private static final BufferedImage GAME_OVER = SpriteUtilities.getGameOverScreen();
	
	/** The game won screen sprite image */
	private static final BufferedImage GAME_WON = SpriteUtilities.getGameWonScreen();
	
	/** The error icon used when displaying save/load errors */
	private static final ImageIcon ERROR_ICON = new ImageIcon("playpanel_sprites/redX.png");
	
	/** The grass-backed icon used for the "new game" button */
	private static final ImageIcon NEW_GRASS = new ImageIcon("selectorpanel_sprites/newGame.png");
	
	/** The grass-backed icon used for the "load game" button */
	private static final ImageIcon LOAD_GRASS = new ImageIcon("selectorpanel_sprites/loadGame.png");
	
	/** The sand-backed icon used for the "new game" button */
	private static final ImageIcon NEW_SAND = new ImageIcon("selectorpanel_sprites/newGame_Over.png");
	
	/** The sand-backed icon used for the "load game" button */
	private static final ImageIcon LOAD_SAND = new ImageIcon("selectorpanel_sprites/loadGame_Over.png");
	
	/** The maximum audio volume */
	private static final int MAX_VOLUME = 100;
	
	/** The default audio volume */
	private static final int DEFAULT_VOLUME = 50;
	
	/** This game frame's selector panel */
	private final SelectorPanel mySelectorPanel;
	
	/** This game frame's layered pane */
	private final JLayeredPane myLayeredPane;
	
	/** This game frame's game panel */
	private GamePanel myGamePanel;

	/** Constructs a new GameFrame which is displayed to the screen. */
	public GameFrame() {
		setTitle("Maze Hops");
		final ImageIcon beerIcon = new ImageIcon("frame_icon/beerIcon.png");
		final Image image = beerIcon.getImage().getScaledInstance(15, -1, Image.SCALE_SMOOTH);
		setIconImage(image);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		final JMenuBar menuBar = new JMenuBar();
		setupMenuBar(menuBar);
		setJMenuBar(menuBar);
		mySelectorPanel = new SelectorPanel();
		myLayeredPane = new JLayeredPane();
		myLayeredPane.setPreferredSize(new Dimension(MazePanel.WIDTH + PlayPanel.WIDTH, MazePanel.HEIGHT));
		myLayeredPane.add(mySelectorPanel, JLayeredPane.DEFAULT_LAYER);
		add(myLayeredPane);
		addComponentListener(new FrameComponentListener());
		setLoopingMusic(SoundType.TITLE);
		pack();
		setLocationRelativeTo(null);
		setResizable(false);
		setVisible(true);
	}
	
	/**
	 * Sets up the menu bar for the GameFrame constructor.
	 * @param theMenuBar passes the JMenuBar to setup.
	 */
	private void setupMenuBar(final JMenuBar theMenuBar) {
		final JMenu fileMenu = new JMenu("File");
		theMenuBar.add(fileMenu);
		
		final JMenu aboutMenu = new JMenu("About");
		theMenuBar.add(aboutMenu);
		
		final JMenu optionsMenu = new JMenu("Options");
		theMenuBar.add(optionsMenu);
			
		// File Menu
		final JMenuItem newGame = new JMenuItem("New Game");
		final JMenuItem saveGame = new JMenuItem("Save Game");
		final JMenuItem loadGame = new JMenuItem("Load Game");
		final JMenuItem exitGame = new JMenuItem("Exit");
		fileMenu.add(newGame);
		fileMenu.add(saveGame);
		fileMenu.add(loadGame);
		fileMenu.addSeparator();
		fileMenu.add(exitGame);
		newGame.addActionListener(theEvent -> newGame());
		saveGame.addActionListener(theEvent -> saveGame());
		loadGame.addActionListener(theEvent -> loadGame());
		exitGame.addActionListener(theEvent -> System.exit(0));
		
		// About Menu
		final JMenuItem aboutGame = new JMenuItem("About");
		aboutMenu.add(aboutGame);
		aboutGame.addActionListener(theEvent -> JOptionPane.showMessageDialog(this,
			    "Maze Hops, University of Washington Tacoma \n"
			    + "TCSS 360 B, Spring 2021 \n"
			    + "Designed by Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy",
			    "About Maze Hops",
			    JOptionPane.PLAIN_MESSAGE));
		
		// Options Menu 
		final JMenu volumeMenu = new JMenu("Adjust Volume");
		final JSlider volumeSlider = new JSlider(0, MAX_VOLUME, DEFAULT_VOLUME);
		volumeSlider.setPaintTrack(true);
		volumeSlider.setMajorTickSpacing(25);
		volumeSlider.setMinorTickSpacing(5);
		volumeSlider.addChangeListener(theEvent -> SoundUtilities.changeVolume(volumeSlider.getValue()));
		volumeMenu.add(volumeSlider);
		optionsMenu.add(volumeMenu);
	}

	@Override
	public void propertyChange(final PropertyChangeEvent theEvent) {
		if (theEvent.getPropertyName().equals(Player.NO_HP) ||
			theEvent.getPropertyName().equals(Maze.END_REACHED)) {
			displaySelectorPanel();
		}
	}
	
	/**
	 * Sets the looping background music for the Maze Hops game.
	 * 
	 * @param theType the sound type of the background music to be played
	 */
	private void setLoopingMusic(final SoundType theType) {
		if (SoundUtilities.isPlaying(SoundType.TITLE) && theType != SoundType.TITLE) {
			SoundUtilities.stop(SoundType.TITLE);
		} else if (SoundUtilities.isPlaying(SoundType.BACKGROUND) && theType != SoundType.BACKGROUND) {
			SoundUtilities.stop(SoundType.BACKGROUND);
		} 
		SoundUtilities.play(theType);
	}
	
	/** Instantiates and displays a new game panel on this game frame. */
	private void newGame() {
		myGamePanel = new GamePanel();
		myGamePanel.addPropertyChangeListener(Player.NO_HP, this);
		myGamePanel.addPropertyChangeListener(Maze.END_REACHED, this);
		myLayeredPane.removeAll();
		myLayeredPane.add(myGamePanel, JLayeredPane.DEFAULT_LAYER);
		myLayeredPane.revalidate();
		myGamePanel.requestFocus();
		setLoopingMusic(SoundType.BACKGROUND);
	}
	
	/** Displays this game frame's selector panel. */
	private void displaySelectorPanel() {
		myLayeredPane.removeAll();
		mySelectorPanel.updateButtonIcons();
		mySelectorPanel.repaint();
		myLayeredPane.add(mySelectorPanel, JLayeredPane.DEFAULT_LAYER);
		myLayeredPane.revalidate();
		if (myGamePanel.isGameWon()) {
			SoundUtilities.play(SoundType.WIN);
		} else {
			SoundUtilities.play(SoundType.LOSE);
		}
		setLoopingMusic(SoundType.TITLE);
	}
	
	/** 
	 * Saves this game frame's game panel to a selected output file. Files are saved
	 * with a .bin extension by default.
	 */
	private void saveGame() {
		final FileDialog fd = new FileDialog(this, "Save Game", FileDialog.SAVE);
		fd.setVisible(true);
		if (fd.getFile() != null) {
			final String fileName = fd.getFile(); 
			try {
				final File f = new File(fd.getDirectory(), fileName);
				final FileOutputStream file = new FileOutputStream(f);
				final ObjectOutputStream out = new ObjectOutputStream(file);
				boolean removed = false;
				for (final Component comp : 
					 myLayeredPane.getComponentsInLayer(JLayeredPane.DEFAULT_LAYER)) {
					if (comp == myGamePanel) {
						removed = true;
						myLayeredPane.remove(myGamePanel);
						break;
					}
				}
				out.writeObject(myGamePanel);
				if (removed) {
					myLayeredPane.add(myGamePanel, JLayeredPane.DEFAULT_LAYER);
					myLayeredPane.revalidate();
				}
				out.close();
				file.close();
			} catch (final Exception ex) {
				showSaveError(ex.getClass().getSimpleName(), fileName);
			} 
		}
	}
	
	/**
	 * Displays a save error message to inform the user that this frame's game panel cannot
	 * be saved.
	 * 
	 * @param theExceptionName the type of exception generated
	 * @param theFileName the name of the chosen output file
	 */
	private void showSaveError(final String theExceptionName, final String theFileName) {
		final String message = String.format("Could not save game: \"%s\"\nSource: %s", 
				                             theFileName, theExceptionName);
		JOptionPane.showMessageDialog(this, message, "Error Saving Game!", 
                                      JOptionPane.ERROR_MESSAGE, ERROR_ICON);
	}
	
	/** Loads a previously saved game panel from a selected input file. */
	private void loadGame() {
		final FileDialog fd = new FileDialog(this, "Load Game", FileDialog.LOAD);
		fd.setVisible(true);
		if (fd.getFile() != null) {
			try {
				final File f = new File(fd.getDirectory(), fd.getFile());
				final FileInputStream file = new FileInputStream(f);
				final ObjectInputStream in = new ObjectInputStream(file);
				myGamePanel = (GamePanel) in.readObject();
				if (myGamePanel.isGameOver() || myGamePanel.isGameWon()) {
					displaySelectorPanel();
				} else {
					myLayeredPane.removeAll();
					myGamePanel.addPropertyChangeListener(this);
					myGamePanel.restoreListeners();
					myLayeredPane.add(myGamePanel, JLayeredPane.DEFAULT_LAYER);
					myLayeredPane.revalidate();
					myGamePanel.requestFocus();
					setLoopingMusic(SoundType.BACKGROUND);
				}
				in.close();
				file.close();
			} catch (final Exception ex) {
				ex.printStackTrace();
				showLoadError(ex.getClass().getSimpleName(), fd.getFile());
			}
		}
	}
	
	/**
	 * Displays a load error message to inform the user that a selected input file cannot
	 * be loaded.
	 * 
	 * @param theExceptionName the type of exception generated
	 * @param theFileName the name of the chosen input file
	 */
	private void showLoadError(final String theExceptionName, final String theFileName) {
		final String message = String.format("Could not load game: \"%s\"\nSource: %s", 
                							 theFileName, theExceptionName);
		JOptionPane.showMessageDialog(this, message, "Error Loading Game!", 
				                      JOptionPane.ERROR_MESSAGE, ERROR_ICON);
	}
	
	/**
	 * SelectorPanel is a class offering components for selecting a new game or loading
	 * a previously saved game. The background image of this panel changes according to the
	 * current state of this game frame's game panel.
	 * 
	 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
	 * @version 31 May 2021
	 */
	private class SelectorPanel extends JPanel {
		
		/** The serial version UID */
		private static final long serialVersionUID = 2071944460810159409L;
		
		/** This selector panel's "new game" button */
		private final JButton myNewButton;
		
		/** This selector panel's "load game" button */
		private final JButton myLoadButton;
		
		/** Creates a new SelectorPanel and configures new/load game buttons. */
		private SelectorPanel() {
			setBounds(0, 0, MazePanel.WIDTH + PlayPanel.WIDTH, MazePanel.HEIGHT);
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
		
		/** 
		 * Updates the new/load game button icons according to the state of this game frame's 
		 * game panel. 
		 */
		private void updateButtonIcons() {
			final ImageIcon newIcon = myGamePanel.isGameWon() ? NEW_GRASS : NEW_SAND;
			final ImageIcon loadIcon = myGamePanel.isGameWon() ? LOAD_GRASS : LOAD_SAND;
			myNewButton.setIcon(newIcon);
			myLoadButton.setIcon(loadIcon);
		}
	}
	
	/**
	 * FrameComponentListener is a class which listens for movements of this game frame
	 * and repaints all child components.
	 * 
	 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
	 * @version 31 May 2021
	 */
	private class FrameComponentListener extends ComponentAdapter {
		
		@Override
		public void componentMoved(final ComponentEvent theEvent) {
			repaint();
		}
	}

}
