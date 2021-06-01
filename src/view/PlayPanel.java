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

public class PlayPanel extends JPanel implements PropertyChangeListener {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2115518979921196291L;
	private static final BufferedImage TRIVIA_IMAGE = SpriteUtilities.getTriviaBackground();
	private static final BufferedImage BACKGROUND = SpriteUtilities.getPlayPanelBackground();
	private static final BufferedImage HOW_TO_PLAY = SpriteUtilities.getHowToPlayImage();
	private static final Font TRIVIA_FONT = new Font(Font.MONOSPACED, Font.BOLD, 20);
	private static final GridBagConstraints GB_CONSTRAINTS = new GridBagConstraints();
	public static final String TRIVIA_ANSWERED = "answered";
	public static final int WIDTH = 530;
	private static final int MAX_BEATS = 5;
	private static final ImageIcon HEART = new ImageIcon("playpanel_sprites/heart.png");
	private static final Color TRANSPARENT = new Color(0, 0, 0, 0);
	private final Set<KeyPadLabel> myKeyLabels;
	private final List<JLabel> myHearts;
	private final TriviaPanel myTriviaPanel;
	private final Timer myHeartTimer;
	private final Player myPlayer;
	private final Maze myMaze;
	private final PropertyChangeSupport myPcs;
	private int myHeartIndex;
	private int myBeatCount;
	private boolean myDisplayingTrivia;
	
	public PlayPanel(final Player thePlayer, final Maze theMaze) {
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
		myHeartIndex = myHearts.size() - 1;
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
			initializeHeartBeat();
		} else if (theEvent.getPropertyName().equals(MazePanel.ADVANCE_DONE)) {
			updateKeyButtons();
		}
	}
	
	public void restoreListeners() {
		myPlayer.restoreListeners();
		myHeartTimer.addActionListener(theEvent -> toggleHeartBeat());
		myTriviaPanel.restoreTimerListeners();
	}
	
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

	public void updateKeyButtons() {
		for (final KeyPadLabel keyLabel : myKeyLabels) {
			keyLabel.updateAppearance(myMaze.isMovementLegal(keyLabel.getMovement()) && 
					                  !myDisplayingTrivia);
		}
	}
	
	public void updateHearts() {
		final int hp = myPlayer.getHealth();
		for (int index = 0; index < Player.MAX_HEALTH; index++) {
			final boolean enabled = index + 1 <= hp ? true : false;
			myHearts.get(index).setEnabled(enabled);
		}
		repaint();
	}

	private void initializeHeartBeat() {
		if (!myHeartTimer.isRunning()) {
			myHeartIndex = myPlayer.getHealth();
			myHeartTimer.start();
		}
	}

	private void toggleHeartBeat() {
		myHearts.get(myHeartIndex).setEnabled(!myHearts.get(myHeartIndex).isEnabled());
		repaint();
		myBeatCount++;
		if (myBeatCount == MAX_BEATS) {
			myHeartTimer.stop();
			myBeatCount = 0;
			updateHearts();
		}
	}
	
	private void setupSouthPanel() {
		final JPanel southPanel = new JPanel(new GridBagLayout());
		southPanel.setPreferredSize(new Dimension(WIDTH, 200));
		southPanel.setBackground(TRANSPARENT);
		southPanel.add(getKeyPanel());
		southPanel.add(Box.createHorizontalStrut(50));
		addHearts(southPanel);
		updateHearts();
		updateKeyButtons();
		add(southPanel, BorderLayout.SOUTH);
	}
	
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
	
	public class TriviaPanel extends JPanel {
		
		/**
		 * 
		 */
		private static final long serialVersionUID = 8731057507725053435L;
		private static final String CORRECT = "Ye got me this time! I won't let you off so easy next time.";
		private static final String INCORRECT = "Argh... not quite! Better luck next time.";
		private static final int HEIGHT = 750;
		private final Timer mySetupTimer;
		private final Timer myTeardownTimer;
		private final TriviaDisplayPanel myDisplayPanel;
		private final Map<TriviaType, JPanel> myAnswerPanels;
		private final List<TriviaComponent> myTriviaComponents;
		private JPanel myAnswerPanel;
		
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
		
		public void processResponse(final String theResponse) {
			Objects.requireNonNull(theResponse, "Responses must be non-null!");
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
		
		private void restoreTimerListeners() {
			mySetupTimer.addActionListener(theEvent -> showTrivia());
			myTeardownTimer.addActionListener(theEvent -> tearDownTrivia());
			for (final TriviaComponent tC : myTriviaComponents) {
				tC.addActionListener(this);
			}
		}
		
		private void setupNewTrivia() {
			final Trivia newTriv = myMaze.getTavernTrivia();
			String question = newTriv.getQuestion();
			if (newTriv.getTriviaType() == TriviaType.MULTICHOICE) {
				question += "\n" + newTriv.getAnswers();
			}
			myDisplayPanel.myTriviaArea.setText(question);
			mySetupTimer.start();
		}
		
		private void tearDownTrivia() {
			myDisplayingTrivia = false;
			myDisplayPanel.updateDisplaySizing();
			clearAnswerPanel();
			myDisplayPanel.myTriviaArea.setVisible(false);
			myPcs.firePropertyChange(TRIVIA_ANSWERED, false, true);
		}
		
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
		
		private void showTrivia() {
			myDisplayPanel.updateDisplaySizing();
			updateAnswerPanel(myMaze.getTavernTrivia().getTriviaType());
			myDisplayPanel.myTriviaArea.setVisible(true);
			setAnswerComponentsActivated(true);
		}
		
		private void updateAnswerPanel(final TriviaType theType) {
			GB_CONSTRAINTS.gridx = 0;
			GB_CONSTRAINTS.gridy = 1;
			GB_CONSTRAINTS.insets = new Insets(50, 0, 0, 0);
			if (getComponentCount() > 1) {
				remove(myAnswerPanel);
			}
			myAnswerPanel = myAnswerPanels.get(theType);
			add(myAnswerPanel, GB_CONSTRAINTS);
			getParent().repaint();
		}
		
		private void clearAnswerPanel() {
			if (getComponentCount() > 1) {
				remove(myAnswerPanel);
			}
			getParent().repaint();
		}
		
		private void setAnswerComponentsActivated(final boolean theActivated) {
			for (final Component comp : myAnswerPanel.getComponents()) {
				comp.setEnabled(theActivated);
			}
		}
	}
	
	private class TriviaDisplayPanel extends JPanel {
		
		/**
		 * 
		 */
		private static final long serialVersionUID = 4287564954541863040L;
		private static final int WIDTH = 475;
		private static final int HEIGHT_ASKING = 595;
		private static final int HEIGHT_NOT_ASKING = 695;
		private static final int BORDER_WIDTH = 4;
		private final JTextArea myTriviaArea;
		
		private TriviaDisplayPanel() {
			updateDisplaySizing();
			setFocusable(false);
			setBorder(BorderFactory.createLineBorder(new Color(255, 171, 66), BORDER_WIDTH));
			add(Box.createRigidArea(new Dimension(WIDTH, 8)));
			myTriviaArea = new JTextArea();
			configureTriviaArea();
			add(myTriviaArea, FlowLayout.CENTER);
		}
		
		public void paintComponent(final Graphics theGraphics) {
			super.paintComponent(theGraphics);
			final Graphics2D g2d = (Graphics2D) theGraphics;
			final BufferedImage image = myDisplayingTrivia ? TRIVIA_IMAGE : HOW_TO_PLAY;
			g2d.drawImage(image, null, 1, 1);
		}
		
		private void updateDisplaySizing() {
			final int height = myDisplayingTrivia ? HEIGHT_ASKING : HEIGHT_NOT_ASKING;
			setPreferredSize(new Dimension(WIDTH, height));
		}
		
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