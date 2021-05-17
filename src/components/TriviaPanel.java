package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Timer;

import model.Maze;
import model.Player;
import model.Trivia;
import view.MazePanel;
import view.PlayPanel;

public class TriviaPanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1600296983489784055L;
	// images to be added later
	private static final String CORRECT = "Correct! Great job!";
	private static final String INCORRECT = "Argh... not quite! Better luck next time!";
	private static final Color BUBBLE_COLOR = new Color(237, 224, 234);
	private static final Font FONT = new Font(Font.MONOSPACED, Font.PLAIN, 15);
	private static final int WIDTH = 475;
	private static final int HEIGHT = 595;
	private static final int BORDER_WIDTH = 4;
	private static TriviaPanel uniqueInstance = new TriviaPanel();
	private final Timer mySetupTimer;
	private final Timer myTeardownTimer;
	private JTextField myTriviaField;
	private Trivia myCurrentTrivia;
	private boolean myDisplayingTrivia;
	
	private TriviaPanel() {
		myTriviaField = new JTextField();
		configureTriviaField();	
		myDisplayingTrivia = false;
		mySetupTimer = new Timer(0, theEvent -> displayTrivia());
		mySetupTimer.setInitialDelay(1000);
		mySetupTimer.setRepeats(false);
		myCurrentTrivia = null;
		myTeardownTimer = new Timer(0, theEvent -> tearDownTrivia());
		myTeardownTimer.setInitialDelay(1000);
		myTeardownTimer.setRepeats(false);
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(false);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		add(Box.createRigidArea(new Dimension(WIDTH, 8)));
		add(myTriviaField, FlowLayout.CENTER);
	}
	
	public static synchronized TriviaPanel getInstance() {
		return uniqueInstance;
	}
	
	public boolean isAsking() {
		return myDisplayingTrivia;
	}
	
	public void setupNewTrivia(final Trivia theTrivia) {
		myDisplayingTrivia = true;
		myCurrentTrivia = theTrivia;
		myTriviaField.setText(theTrivia.getQuestion());
		mySetupTimer.start();
	}
	
	public void processResponse(final String theResponse) {
		if (myCurrentTrivia.isCorrect(theResponse)) {
			myTriviaField.setText(CORRECT);
			Maze.getInstance().removeTavern();
		} else {
			myTriviaField.setText(INCORRECT);
			PlayPanel.getInstance().initializeHeartBeat();
			Player.getInstance().decrementHealth();
		}
		myTeardownTimer.start();
	}
	
	private void tearDownTrivia() {
		myTriviaField.setVisible(false);
		revalidate();
		PlayPanel.getInstance().clearAnswerPanel();
		myDisplayingTrivia = false;
		MazePanel.getInstance().restoreVisibility(myCurrentTrivia.isAnswered());
	}
	
	private void configureTriviaField() {
		myTriviaField.setEditable(false);
		myTriviaField.setPreferredSize(new Dimension(WIDTH - 40, HEIGHT / 3));
		myTriviaField.setBorder(BorderFactory.createLineBorder(Color.BLACK, 3));
		myTriviaField.setBackground(BUBBLE_COLOR);
		myTriviaField.setFont(FONT);
		myTriviaField.setEditable(false);
		myTriviaField.setFocusable(false);
		myTriviaField.setVisible(false);
	}
	
	private void displayTrivia() {
		myTriviaField.setVisible(true);
		revalidate();
		PlayPanel.getInstance().updateAnswerPanel(myCurrentTrivia.getTriviaType());
	}
	
}
