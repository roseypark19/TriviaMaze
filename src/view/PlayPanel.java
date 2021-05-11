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
import javax.swing.JPanel;
import components.AnswerField;
import components.Heart;
import components.KeyPadButton;
import components.MultiChoiceButton;
import components.QuestionField;
import components.ShortAnswerField;
import components.TrueFalseButton;
import model.Movement;
import model.Player;
import model.QuestionType;

public class PlayPanel extends JPanel {
	
	private static final int WIDTH = 530;
	private static final int HEIGHT = MazePanel.HEIGHT;
	private static final Color BACKGROUND = new Color(217, 179, 130);
	private static final Color TRANSPARENT = new Color(0, 0, 0, 0);
	private static PlayPanel uniqueInstance = new PlayPanel();
	private final Map<QuestionType, JPanel> myAnswerPanels;
	private final QuestionField myQuestionField;
	private final AnswerField myAnswerField;
	private final Set<KeyPadButton> myKeyButtons;
	private final List<Heart> myHearts;
	private final JPanel myNorthPanel;
	private final JPanel myCenterPanel;
	private final JPanel mySouthPanel;
	private JPanel myAnswerPanel;
	
	private PlayPanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setBackground(BACKGROUND);
		setLayout(new BorderLayout());
		myNorthPanel = new JPanel(new GridBagLayout());
		myQuestionField = new QuestionField();
		configureNorthPanel();
		add(myNorthPanel, BorderLayout.NORTH);
		myCenterPanel = new JPanel(new GridBagLayout());
		myAnswerPanels = new HashMap<>();
		myAnswerField = new AnswerField();
		configureCenterPanel();
		add(myCenterPanel, BorderLayout.CENTER);
		mySouthPanel = new JPanel(new GridBagLayout());
		myKeyButtons = new HashSet<>();
		myHearts = new ArrayList<>();
		configureSouthPanel();
		add(mySouthPanel, BorderLayout.SOUTH);
	}
	
	public static synchronized PlayPanel getInstance() {
		return uniqueInstance;
	}
	
	@Override
	public void paintComponent(final Graphics theGraphics) {
		super.paintComponent(theGraphics);
	}
	
	private void configureNorthPanel() {
		myNorthPanel.setPreferredSize(new Dimension(WIDTH, 400));
		myNorthPanel.setBackground(TRANSPARENT);
		myNorthPanel.add(myQuestionField);
	}
	
	private void configureCenterPanel() {
		myCenterPanel.setPreferredSize(new Dimension(WIDTH, 300));
		myCenterPanel.setBackground(TRANSPARENT);
		myCenterPanel.add(myAnswerField);
		populateAnswerMap();
		updateAnswerPanel(QuestionType.NONE);
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
		char letter = 'A';
		for (int value = 1; value <= 4; value++) {
			multiChoice.add(new MultiChoiceButton(letter++));
			if (value < 4) {
				multiChoice.add(Box.createHorizontalStrut(20));
			}
		}
		myAnswerPanels.put(QuestionType.MULTICHOICE, multiChoice);
		final JPanel trueFalse = new JPanel(new GridBagLayout());
		trueFalse.setBackground(TRANSPARENT);
		trueFalse.add(new TrueFalseButton(true));
		trueFalse.add(Box.createHorizontalStrut(20));
		trueFalse.add(new TrueFalseButton(false));
		myAnswerPanels.put(QuestionType.TRUEFALSE, trueFalse);
		final JPanel shortAns = new JPanel(new GridBagLayout());
		shortAns.add(new ShortAnswerField());
		myAnswerPanels.put(QuestionType.SHORTANSWER, shortAns);
		myAnswerPanels.put(QuestionType.SHORTANSWER, shortAns);
		final JPanel noAnswerType = new JPanel();
		noAnswerType.setBackground(TRANSPARENT);
		noAnswerType.setPreferredSize(new Dimension(0, 45));
		noAnswerType.setFocusable(false);
		myAnswerPanels.put(QuestionType.NONE, noAnswerType);
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
			gbc.gridx = gridXs[index];
			gbc.gridy = gridYs[index];
			buttonPanel.add(b, gbc);
			index++;
		}
		return buttonPanel;
	}
	
	private void addHearts() {
		for (int count = 1; count <= Player.MAX_HEALTH; count++) {
			final Heart heart = new Heart();
			mySouthPanel.add(heart);
			myHearts.add(heart);
			if (count < Player.MAX_HEALTH) {
				mySouthPanel.add(Box.createHorizontalStrut(10));
			}
		}
	}
	
	public void updateKeyButtons() {
		for (final KeyPadButton button : myKeyButtons) {
			button.updateAppearance(MazePanel.getInstance().
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
	
	public void updateAnswerPanel(final QuestionType theType) {
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.insets = new Insets(25, 0, 0, 0);
		if (myCenterPanel.getComponentCount() > 1) {
			myCenterPanel.remove(myAnswerPanel);
		}
		myAnswerPanel = myAnswerPanels.get(theType);
		myCenterPanel.add(myAnswerPanel, gbc);
	}
}