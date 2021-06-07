/*
 * PlayPanel.java
 * TCSS360 - Trivia Maze
 * Spring 2021
 */

package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.Timer;

import components.KeyPadLabel;
import components.MultiChoiceButton;
import components.ShortAnswerField;
import components.TriviaComponent;
import components.TrueFalseButton;
import model.Maze;
import model.Movement;
import model.Player;
import model.SoundType;
import model.Trivia;
import model.TriviaType;
import utilities.SoundUtilities;
import utilities.SpriteUtilities;

/**
 * PlayPanel is a class which displays Maze Hops trivia questions as well as the player's
 * valid movement directions and current health.
 * 
 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
 * @version 31 May 2021
 */
public class PlayPanel extends JPanel implements PropertyChangeListener {
	
	/** The serial version UID */
	private static final long serialVersionUID = 2115518979921196291L;
	
	/** The background sprite images used for displaying trivia */
	private static final BufferedImage TRIVIA_IMAGE = SpriteUtilities.getTriviaBackground();
	
	/** The background images of a play panel */
	private static final BufferedImage BACKGROUND = SpriteUtilities.getPlayPanelBackground();
	
	/** The "how to play" image displayed when the user is not answering trivia */
	private static final BufferedImage HOW_TO_PLAY = SpriteUtilities.getHowToPlayImage();
	
	/** The font used for trivia questions */
	private static final Font TRIVIA_FONT = new Font(Font.MONOSPACED, Font.BOLD, 20);
	
	/** The grid bag constraints object used for component layout */
	private static final GridBagConstraints GB_CONSTRAINTS = new GridBagConstraints();
	
	/** The bound property indicating that a trivia question has been answered */
	public static final String TRIVIA_ANSWERED = "answered";
	
	/** A play panel's width */
	public static final int WIDTH = 530;
	
	/** The number of heart beats displayed when the player loses a health */
	private static final int MAX_BEATS = 5;
	
	/** The heart image icon */
	private static final ImageIcon HEART = new ImageIcon("playpanel_sprites/heart.png");
	
	/** The transparent background color */
	private static final Color TRANSPARENT = new Color(0, 0, 0, 0);
	
	/** The set of WASD key pad labels on this play panel */
	private final Set<KeyPadLabel> myKeyLabels;
	
	/** The list of hearts on this play panel */
	private final List<JLabel> myHearts;
	
	/** This play panel's trivia panel */
	private final TriviaPanel myTriviaPanel;
	
	/** The timer used to control heart beat animations */
	private final Timer myHeartTimer;
	
	/** This play panel's player */
	private final Player myPlayer;
	
	/** This play panel's maze */
	private final Maze myMaze;
	
	/** The property change support object which fires bound property changes for this play panel */
	private final PropertyChangeSupport myPcs;
	
	/** The number of heart beats performed */
	private int myBeatCount;
	
	/** Indicates whether or not this play panel is currently displaying a trivia question */
	private boolean myDisplayingTrivia;
	
	/**
	 * Constructs a new PlayPanel, assigning the specified player and maze.
	 * 
	 * @param thePlayer the player to be assigned
	 * @param theMaze the maze to be assigned 
	 * @throws NullPointerException if thePlayer or theMaze is null
	 */
	PlayPanel(final Player thePlayer, final Maze theMaze) {
		Objects.requireNonNull(thePlayer, "Players must be non-null!");
		Objects.requireNonNull(theMaze, "Mazes must be non-null!");
		setPreferredSize(new Dimension(WIDTH, MazePanel.HEIGHT));
		setLayout(new BorderLayout());
		myPlayer = thePlayer;
		myMaze = theMaze;
		myPcs = new PropertyChangeSupport(this);
		myTriviaPanel = new TriviaPanel();
		add(myTriviaPanel, BorderLayout.NORTH);
		myKeyLabels = new HashSet<>();
		myHearts = new ArrayList<>();
		setupSouthPanel();
		myHeartTimer = new Timer(200, theEvent -> toggleHeartBeat());
		myBeatCount = 0;
		myDisplayingTrivia = false;
	}
	
	@Override
	public void propertyChange(final PropertyChangeEvent theEvent) {
		if (theEvent.getPropertyName().equals(MazePanel.INSIDE_TAVERN)) {
			myDisplayingTrivia = true;
			myTriviaPanel.setupNewTrivia();
		} else if (theEvent.getPropertyName().equals(Player.HEALTH_GAINED)) {
			updateHearts();
		} else if (theEvent.getPropertyName().equals(Player.HEALTH_LOST)) {
			myHeartTimer.start();
		} else if (theEvent.getPropertyName().equals(MazePanel.ADVANCE_DONE)) {
			updateKeyLabels();
		}
	}
	
	/** Restores the action listeners associated with this play panel. */
	void restoreListeners() {
		myPlayer.restoreListeners();
		myHeartTimer.addActionListener(theEvent -> toggleHeartBeat());
		myTriviaPanel.restoreListeners();
	}
	
	/**
	 * Indicates whether or not this play panel is currently animating its components.
	 * 
	 * @return true if animation is in process, false otherwise
	 */
	boolean isAnimating() {
		return myTriviaPanel.mySetupTimer.isRunning() || myTriviaPanel.myTeardownTimer.isRunning();
	}
	
	/**
	 * Adds a property change listener to this play panel which listens for changes in bound
	 * properties.
	 * 
	 * @param theListener the property change listener to be assigned
	 * @throws NullPointerException if theListener is null
	 */
	public void addPropertyChangeListener(final PropertyChangeListener theListener) {
		Objects.requireNonNull(theListener, "Property change listeners must be non-null!");
		myPcs.addPropertyChangeListener(theListener);
	}
	
	@Override
	public void paintComponent(final Graphics theGraphics) {
		super.paintComponent(theGraphics);
		final Graphics2D g2d = (Graphics2D) theGraphics;
		g2d.drawImage(BACKGROUND, null, 0, 0);
	}

	/** Updates the WASD key labels according to the valid movement directions of this play panel's maze. */
	private void updateKeyLabels() {
		for (final KeyPadLabel keyLabel : myKeyLabels) {
			keyLabel.updateAppearance(myMaze.isMovementLegal(keyLabel.getMovement()) && 
					                  !myDisplayingTrivia);
		}
	}
	
	/** Updates the heart labels according to this play panel's player's health. */
	private void updateHearts() {
		final int hp = myPlayer.getHealth();
		for (int index = 0; index < Player.MAX_HEALTH; index++) {
			final boolean enabled = index + 1 <= hp ? true : false;
			myHearts.get(index).setEnabled(enabled);
		}
		repaint();
	}

	/** Performs a single heart beat animation. */
	private void toggleHeartBeat() {
		final int hp = myPlayer.getHealth();
		myHearts.get(hp).setEnabled(!myHearts.get(hp).isEnabled());
		repaint();
		myBeatCount++;
		if (myBeatCount == MAX_BEATS) {
			myHeartTimer.stop();
			myBeatCount = 0;
			updateHearts();
		}
	}
	
	/** Configures this play panel's south panel which contains the WASD key labels and heart labels. */
	private void setupSouthPanel() {
		final JPanel southPanel = new JPanel(new GridBagLayout());
		southPanel.setPreferredSize(new Dimension(WIDTH, 200));
		southPanel.setBackground(TRANSPARENT);
		southPanel.add(getKeyPanel());
		southPanel.add(Box.createHorizontalStrut(50));
		addHearts(southPanel);
		updateHearts();
		updateKeyLabels();
		add(southPanel, BorderLayout.SOUTH);
	}
	
	/** Adds heart labels to this play panel's south panel. */
	private void addHearts(final JPanel theSouthPanel) {
		for (int count = 1; count <= Player.MAX_HEALTH; count++) {
			final JLabel heart = new JLabel(HEART);
			theSouthPanel.add(heart);
			myHearts.add(heart);
			if (count < Player.MAX_HEALTH) {
				theSouthPanel.add(Box.createHorizontalStrut(10));
			}
		}
	}
	
	/** Creates and supplies the panel containing this play panel's WASD key labels. */
	private JPanel getKeyPanel() {
		final JPanel buttonPanel = new JPanel(new GridBagLayout());
		buttonPanel.setBackground(TRANSPARENT);
		final int[] gridXs = new int[] {1, 0, 1, 2};
		final int[] gridYs = new int[] {0, 1, 1, 1};
		GB_CONSTRAINTS.insets = new Insets(3, 3, 3, 3);
		int index = 0;
		for (final Movement move : Movement.values()) {
			final KeyPadLabel l = new KeyPadLabel(move);
			myKeyLabels.add(l);
			GB_CONSTRAINTS.gridx = gridXs[index];
			GB_CONSTRAINTS.gridy = gridYs[index];
			buttonPanel.add(l, GB_CONSTRAINTS);
			index++;
		}
		return buttonPanel;
	}
	
	/**
	 * TriviaPanel is a class which contains a trivia display panel for displaying trivia as
	 * well as all corresponding trivia components for entering responses.
	 * 
	 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
	 * @version 31 May 2021
	 */
	public class TriviaPanel extends JPanel {
		
		/** The serial version UID */
		private static final long serialVersionUID = 8731057507725053435L;
		
		/** The message displayed when a trivia question is answered correctly */
		private static final String CORRECT = "Ye got me this time! I won't let you off so easy next time.";
		
		/** The message displayed when a trivia question is answered incorrectly */
		private static final String INCORRECT = "Argh... not quite! Better luck next time.";
		
		/** The height of a trivia panel */
		private static final int HEIGHT = 750;
		
		/** The timer used for setting up trivia questions and corresponding components */
		private final Timer mySetupTimer;
		
		/** The timer used for tearing down trivia questions and corresponding components */
		private final Timer myTeardownTimer;
		
		/** This trivia panel's trivia display panel */
		private final TriviaDisplayPanel myDisplayPanel;
		
		/** A mapping of each trivia type to this trivia panel's corresponding trivia component panels */
		private final Map<TriviaType, JPanel> myAnswerPanels;
		
		/** The list of all trivia components associated with this trivia panel */
		private final List<TriviaComponent> myTriviaComponents;
		
		/** The panel of trivia components currently being displayed by this trivia panel */
		private JPanel myAnswerPanel;
		
		/** Creates a new TriviaPanel and configures all child components. */
		private TriviaPanel() {
			setLayout(new GridBagLayout());
			setPreferredSize(new Dimension(PlayPanel.WIDTH, HEIGHT));
			setBackground(TRANSPARENT);
			mySetupTimer = new Timer(0, theEvent -> showTrivia());
			mySetupTimer.setInitialDelay(800);
			mySetupTimer.setRepeats(false);
			myTeardownTimer = new Timer(0, theEvent -> tearDownTrivia());
			myTeardownTimer.setInitialDelay(1200);
			myTeardownTimer.setRepeats(false);
			myDisplayPanel = new TriviaDisplayPanel();
			add(myDisplayPanel);
			myAnswerPanels = new HashMap<>();
			myTriviaComponents = new ArrayList<>();
			populateAnswerMap();
		}
		
		/**
		 * Processes a provided response to the current trivia question being displayed.
		 * 
		 * @param theResponse the response to be processed
		 * @throws NullPointerException if theResponse is null
		 * @throws IllegalStateException if a trivia question is not being displayed by this
		 *         trivia panel
		 */
		public void processResponse(final String theResponse) {
			Objects.requireNonNull(theResponse, "Responses must be non-null!");
			if (!myDisplayingTrivia) {
				throw new IllegalStateException("A trivia question is not being displayed!");
			}
			setAnswerComponentsActivated(false);
			if (myMaze.getTavernTrivia().isCorrect(theResponse)) {
				SoundUtilities.play(SoundType.CORRECT);
				myDisplayPanel.myTriviaArea.setText(CORRECT);
			} else {
				SoundUtilities.play(SoundType.INCORRECT);
				myDisplayPanel.myTriviaArea.setText(INCORRECT);
				myPlayer.decrementHealth();
			}
			myTeardownTimer.start();
		}
		
		/** Restores the action listeners associated with this trivia panel. */
		private void restoreListeners() {
			mySetupTimer.addActionListener(theEvent -> showTrivia());
			myTeardownTimer.addActionListener(theEvent -> tearDownTrivia());
			for (final TriviaComponent tC : myTriviaComponents) {
				tC.addActionListener(this);
			}
		}
		
		/** Sets up and displays the trivia question of this play panel's maze tavern. */
		private void setupNewTrivia() {
			final Trivia newTriv = myMaze.getTavernTrivia();
			String question = newTriv.getQuestion();
			if (newTriv.getTriviaType() == TriviaType.MULTICHOICE) {
				question += "\n" + newTriv.getAnswers();
			}
			myDisplayPanel.myTriviaArea.setText(question);
			mySetupTimer.start();
		}
		
		/** Tears down the trivia question of this play panel's maze tavern. */
		private void tearDownTrivia() {
			myDisplayingTrivia = false;
			myDisplayPanel.updateDisplaySizing();
			clearAnswerPanel();
			myDisplayPanel.myTriviaArea.setVisible(false);
			myPcs.firePropertyChange(TRIVIA_ANSWERED, false, true);
		}
		
		/** Configures and adds all trivia components to this trivia panel's trivia type panel mapping. */
		private void populateAnswerMap() {
			final JPanel multiChoice = new JPanel(new GridBagLayout());
			multiChoice.setBackground(TRANSPARENT);
			for (char letter = 'A'; letter <= 'D'; letter++) {
				final MultiChoiceButton mC = new MultiChoiceButton(letter, this);
				myTriviaComponents.add(mC);
				multiChoice.add(mC);
				if (letter < 'D') {
					multiChoice.add(Box.createHorizontalStrut(40));
				}
			}
			myAnswerPanels.put(TriviaType.MULTICHOICE, multiChoice);
			final JPanel trueFalse = new JPanel(new GridBagLayout());
			trueFalse.setBackground(TRANSPARENT);
			final TrueFalseButton trueButton = new TrueFalseButton(true, this);
			myTriviaComponents.add(trueButton);
			trueFalse.add(trueButton);
			trueFalse.add(Box.createHorizontalStrut(40));
			final TrueFalseButton falseButton = new TrueFalseButton(false, this);
			myTriviaComponents.add(falseButton);
			trueFalse.add(falseButton);
			myAnswerPanels.put(TriviaType.TRUEFALSE, trueFalse);
			final JPanel shortAns = new JPanel(new GridBagLayout());
			final ShortAnswerField sA = new ShortAnswerField(this);
			myTriviaComponents.add(sA);
			shortAns.add(sA);
			myAnswerPanels.put(TriviaType.SHORTANSWER, shortAns);
		}
		
		/** Displays this trivia panel's trivia display panel and corresponding trivia components. */
		private void showTrivia() {
			myDisplayPanel.updateDisplaySizing();
			updateAnswerPanel(myMaze.getTavernTrivia().getTriviaType());
			myDisplayPanel.myTriviaArea.setVisible(true);
			setAnswerComponentsActivated(true);
		}
		
		/**
		 * Updates this trivia panel's trivia component panel according to the provided trivia type. 
		 * 
		 * @param theType the trivia type
		 */
		private void updateAnswerPanel(final TriviaType theType) {
			GB_CONSTRAINTS.gridx = 0;
			GB_CONSTRAINTS.gridy = 1;
			GB_CONSTRAINTS.insets = new Insets(50, 0, 0, 0);
			myAnswerPanel = myAnswerPanels.get(theType);
			add(myAnswerPanel, GB_CONSTRAINTS);
			getParent().repaint();
		}
		
		/** Clears this trivia panel's current trivia component panel from view. */
		private void clearAnswerPanel() {
			remove(myAnswerPanel);
			getParent().repaint();
		}
		
		/**
		 * Activates/deactivates the trivia components on this trivia panel.
		 * 
		 * @param theActivated a true/false activation indication
		 */
		private void setAnswerComponentsActivated(final boolean theActivated) {
			for (final Component comp : myAnswerPanel.getComponents()) {
				comp.setEnabled(theActivated);
			}
		}
	}
	
	/**
	 * TriviaDisplayPanel is a class which displays trivia questions backed by a custom
	 * character image. When this play panel is not displaying trivia, a trivia display
	 * panel serves the purpose of displaying the "how to play" image to the user.
	 * 
	 * @author Parker Rosengreen, Rebekah Parkhurst, Artem Potafiy
	 * @version 31 May 2021
	 */
	private class TriviaDisplayPanel extends JPanel {
		
		/** The serial version UID */
		private static final long serialVersionUID = 4287564954541863040L;
		
		/** A trivia display panel's width */
		private static final int WIDTH = 475;
		
		/** A trivia display panel's height when displaying a trivia question */
		private static final int HEIGHT_ASKING = 595;
		
		/** A trivia display panel's height when not displaying a trivia question */
		private static final int HEIGHT_NOT_ASKING = 695;
		
		/** A trivia display panel's border width */
		private static final int BORDER_WIDTH = 5;
		
		/** A trivia display panel's text area for printing trivia questions */
		private final JTextArea myTriviaArea;
		
		/** Creates and configures a new TriviaDisplayPanel */
		private TriviaDisplayPanel() {
			updateDisplaySizing();
			setFocusable(false);
			setBorder(BorderFactory.createLineBorder(new Color(255, 171, 66), BORDER_WIDTH));
			add(Box.createRigidArea(new Dimension(WIDTH, 8)));
			myTriviaArea = new JTextArea();
			configureTriviaArea();
			add(myTriviaArea, FlowLayout.CENTER);
		}
		
		@Override
		public void paintComponent(final Graphics theGraphics) {
			super.paintComponent(theGraphics);
			final Graphics2D g2d = (Graphics2D) theGraphics;
			final BufferedImage image = myDisplayingTrivia ? TRIVIA_IMAGE : HOW_TO_PLAY;
			g2d.drawImage(image, null, 0, 0);
		}
		
		/** 
		 * Updates this trivia display panel's sizing based on if this play panel is 
		 * displaying a trivia question 
		 */
		private void updateDisplaySizing() {
			final int height = myDisplayingTrivia ? HEIGHT_ASKING : HEIGHT_NOT_ASKING;
			setPreferredSize(new Dimension(WIDTH, height));
		}
		
		/** Configures this trivia display panel's trivia question text area. */
		private void configureTriviaArea() {
			myTriviaArea.setEditable(false);
			myTriviaArea.setPreferredSize(new Dimension(WIDTH - 40, HEIGHT_ASKING / 3));
			myTriviaArea.setMargin(new Insets(5, 5, 5, 5));
			myTriviaArea.setBackground(Color.WHITE);
			myTriviaArea.setFont(TRIVIA_FONT);
			myTriviaArea.setLineWrap(true);
			myTriviaArea.setWrapStyleWord(true);
			myTriviaArea.setEditable(false);
			myTriviaArea.setFocusable(false);
			myTriviaArea.setVisible(false);
		}
	}
	
}