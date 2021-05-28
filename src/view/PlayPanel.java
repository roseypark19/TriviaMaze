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
	private static final Font TRIVIA_FONT = new Font(Font.MONOSPACED, Font.BOLD, 20);
	private static final GridBagConstraints GB_CONSTRAINTS = new GridBagConstraints();
	public static final String TRIVIA_ANSWERED = "answered";
	public static final int WIDTH = 530;
	private static final int MAX_BEATS = 5;
	private static final ImageIcon HEART = new ImageIcon("heart.png");
	private static final Color BACKGROUND = new Color(217, 179, 130);
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
		setPreferredSize(new Dimension(WIDTH, MazePanel.HEIGHT));
		setBackground(BACKGROUND);
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
	
	public void addPropertyChangeListener(final PropertyChangeListener theListener) {
		myPcs.addPropertyChangeListener(theListener);
	}
	
	@Override
	public void paintComponent(final Graphics theGraphics) {
		super.paintComponent(theGraphics);
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
			myHeartIndex = Math.max(0, myPlayer.getHealth() - 1);
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
		private JPanel myAnswerPanel;
		private Trivia myCurrentTrivia;
		
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
			populateAnswerMap();
		}
		
		public void processResponse(final String theResponse) {
			setAnswerComponentsActivated(false);
			if (myCurrentTrivia.isCorrect(theResponse)) {
				SoundUtilities.play(SoundType.CORRECT);
				myDisplayPanel.myTriviaArea.setText(CORRECT);
			} else {
				SoundUtilities.play(SoundType.INCORRECT);
				myDisplayPanel.myTriviaArea.setText(INCORRECT);
				initializeHeartBeat();
				myPlayer.decrementHealth();
			}
			myTeardownTimer.start();
		}
		
		private void setupNewTrivia() {
			myCurrentTrivia = myMaze.getTavernTrivia();
			String question = myCurrentTrivia.getQuestion();
			if (myCurrentTrivia.getTriviaType() == TriviaType.MULTICHOICE) {
				question += "\n" + myCurrentTrivia.getAnswers();
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
				multiChoice.add(new MultiChoiceButton(letter, this));
				if (letter < 'D') {
					multiChoice.add(Box.createHorizontalStrut(40));
				}
			}
			myAnswerPanels.put(TriviaType.MULTICHOICE, multiChoice);
			final JPanel trueFalse = new JPanel(new GridBagLayout());
			trueFalse.setBackground(TRANSPARENT);
			trueFalse.add(new TrueFalseButton(true, this));
			trueFalse.add(Box.createHorizontalStrut(40));
			trueFalse.add(new TrueFalseButton(false, this));
			myAnswerPanels.put(TriviaType.TRUEFALSE, trueFalse);
			final JPanel shortAns = new JPanel(new GridBagLayout());
			shortAns.add(new ShortAnswerField(this));
			myAnswerPanels.put(TriviaType.SHORTANSWER, shortAns);
		}
		
		private void showTrivia() {
			myDisplayPanel.updateDisplaySizing();
			updateAnswerPanel(myCurrentTrivia.getTriviaType());
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
			setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
			add(Box.createRigidArea(new Dimension(WIDTH, 8)));
			myTriviaArea = new JTextArea();
			configureTriviaArea();
			add(myTriviaArea, FlowLayout.CENTER);
		}
		
		public void paintComponent(final Graphics theGraphics) {
			super.paintComponent(theGraphics);
			final Graphics2D g2d = (Graphics2D) theGraphics;
			final BufferedImage image = myDisplayingTrivia ? TRIVIA_IMAGE : null;
			g2d.drawImage(image, null, 0, 0);
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