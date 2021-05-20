package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Timer;

import components.KeyPadButton;
import components.MultiChoiceButton;
import components.ShortAnswerField;
import components.TriviaPanel;
import components.TrueFalseButton;
import model.Maze;
import model.Movement;
import model.Player;
import model.TriviaType;

public class PlayPanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2115518979921196291L;
	private static final GridBagConstraints GB_CONSTRAINTS = new GridBagConstraints();
	public static final int WIDTH = 530;
	public static final int HEIGHT = MazePanel.HEIGHT;
	private static final int MAX_BEATS = 5;
	private static final ImageIcon HEART = new ImageIcon("heart.png");
	private static final Color BACKGROUND = new Color(217, 179, 130);
	private static final Color TRANSPARENT = new Color(0, 0, 0, 0);
	private static PlayPanel uniqueInstance = new PlayPanel();
	private final Map<TriviaType, JPanel> myAnswerPanels;
	private final Set<KeyPadButton> myKeyButtons;
	private final List<JLabel> myHearts;
	private final JPanel myNorthPanel;
	private final JPanel mySouthPanel;
	private final JPanel myNoTriviaPanel;
	private JPanel myAnswerPanel;
	private final Timer myHeartTimer;
	private int myHeartIndex;
	private int myBeatCount;
	
	private PlayPanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setBackground(BACKGROUND);
		setLayout(new BorderLayout());
		myNorthPanel = new JPanel(new GridBagLayout());
		myAnswerPanels = new HashMap<>();
		myNoTriviaPanel = new JPanel();
		configureNorthPanel();
		add(myNorthPanel, BorderLayout.NORTH);
		mySouthPanel = new JPanel(new GridBagLayout());
		myKeyButtons = new HashSet<>();
		myHearts = new ArrayList<>();
		configureSouthPanel();
		add(mySouthPanel, BorderLayout.SOUTH);
		myHeartTimer = new Timer(200, theEvent -> toggleHeartBeat());
		myHeartIndex = myHearts.size() - 1;
		myBeatCount = 0;
	}
	
	public static synchronized PlayPanel getInstance() {
		return uniqueInstance;
	}
	
	@Override
	public void paintComponent(final Graphics theGraphics) {
		super.paintComponent(theGraphics);
	}
	
	private void configureNorthPanel() {
		myNorthPanel.setPreferredSize(new Dimension(WIDTH, 750));
		myNorthPanel.setBackground(TRANSPARENT);
		myNorthPanel.add(TriviaPanel.getInstance());
		populateAnswerMap();
		clearAnswerPanel();
	}
	
	private void configureSouthPanel() {
		mySouthPanel.setPreferredSize(new Dimension(WIDTH, 200));
		mySouthPanel.setBackground(TRANSPARENT);
		mySouthPanel.add(getKeyPanel());
		updateKeyButtons();
		mySouthPanel.add(Box.createHorizontalStrut(50));
		addHearts();
		updateHearts();
	}
	
	private void populateAnswerMap() {
		final JPanel multiChoice = new JPanel(new GridBagLayout());
		multiChoice.setBackground(TRANSPARENT);
		for (char letter = 'A'; letter <= 'D'; letter++) {
			multiChoice.add(new MultiChoiceButton(letter));
			if (letter < 'D') {
				multiChoice.add(Box.createHorizontalStrut(40));
			}
		}
		myAnswerPanels.put(TriviaType.MULTICHOICE, multiChoice);
		final JPanel trueFalse = new JPanel(new GridBagLayout());
		trueFalse.setBackground(TRANSPARENT);
		trueFalse.add(new TrueFalseButton(true));
		trueFalse.add(Box.createHorizontalStrut(40));
		trueFalse.add(new TrueFalseButton(false));
		myAnswerPanels.put(TriviaType.TRUEFALSE, trueFalse);
		final JPanel shortAns = new JPanel(new GridBagLayout());
		shortAns.add(new ShortAnswerField());
		myAnswerPanels.put(TriviaType.SHORTANSWER, shortAns);
		myNoTriviaPanel.setBackground(TRANSPARENT);
		myNoTriviaPanel.setPreferredSize(new Dimension(0, 55));
		myNoTriviaPanel.setFocusable(false);
	}
	
	private JPanel getKeyPanel() {
		final JPanel buttonPanel = new JPanel(new GridBagLayout());
		buttonPanel.setBackground(TRANSPARENT);
		final int[] gridXs = new int[] {1, 0, 1, 2};
		final int[] gridYs = new int[] {0, 1, 1, 1};
		GB_CONSTRAINTS.insets = new Insets(3, 3, 3, 3);
		int index = 0;
		for (final Movement move : Movement.values()) {
			final KeyPadButton b = new KeyPadButton(move);
			myKeyButtons.add(b);
			GB_CONSTRAINTS.gridx = gridXs[index];
			GB_CONSTRAINTS.gridy = gridYs[index];
			buttonPanel.add(b, GB_CONSTRAINTS);
			index++;
		}
		return buttonPanel;
	}
	
	private void addHearts() {
		for (int count = 1; count <= Player.MAX_HEALTH; count++) {
			final JLabel heart = new JLabel(HEART);
			mySouthPanel.add(heart);
			myHearts.add(heart);
			if (count < Player.MAX_HEALTH) {
				mySouthPanel.add(Box.createHorizontalStrut(10));
			}
		}
	}
	
	public void updateKeyButtons() {
		for (final KeyPadButton button : myKeyButtons) {
			button.updateAppearance(Maze.getInstance().
					                isMovementLegal(button.getMovement()));
		}
	}
	
	public void updateHearts() {
		final int hp = Player.getInstance().getHealth();
		for (int index = 0; index < Player.MAX_HEALTH; index++) {
			final boolean enabled = index + 1 <= hp ? true : false;
			myHearts.get(index).setEnabled(enabled);
		}
		repaint();
	}
	
	public void updateAnswerPanel(final TriviaType theType) {
		GB_CONSTRAINTS.gridx = 0;
		GB_CONSTRAINTS.gridy = 1;
		GB_CONSTRAINTS.insets = new Insets(50, 0, 0, 0);
		if (myNorthPanel.getComponentCount() > 1) {
			myNorthPanel.remove(myAnswerPanel);
		}
		myAnswerPanel = myAnswerPanels.get(theType);
		myNorthPanel.add(myAnswerPanel, GB_CONSTRAINTS);
		repaint();
	}
	
	public void clearAnswerPanel() {
		GB_CONSTRAINTS.gridx = 0;
		GB_CONSTRAINTS.gridy = 1;
		GB_CONSTRAINTS.insets = new Insets(50, 0, 0, 0);
		if (myNorthPanel.getComponentCount() > 1) {
			myNorthPanel.remove(myAnswerPanel);
		}
		myAnswerPanel = myNoTriviaPanel;
		myNorthPanel.add(myAnswerPanel, GB_CONSTRAINTS);
		repaint();
	}
	
	public void initializeHeartBeat() {
		if (!myHeartTimer.isRunning()) {
			myHeartIndex = Math.max(0, Player.getInstance().getHealth() - 1);
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
}