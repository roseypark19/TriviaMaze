package components;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JPanel;
import javax.swing.JTextArea;
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
	private static final String INCORRECT = "Argh... not quite! Better luck next time.";
	private static final Font FONT = new Font(Font.MONOSPACED, Font.BOLD, 20);
	private static final int WIDTH = 475;
	private static final int HEIGHT = 595;
	private static final int BORDER_WIDTH = 4;
	private static TriviaPanel uniqueInstance = new TriviaPanel();
	private final Timer mySetupTimer;
	private final Timer myTeardownTimer;
	private JTextArea myTriviaArea;
	private Trivia myCurrentTrivia;
	private boolean myDisplayingTrivia;
	
	private TriviaPanel() {
		myTriviaArea = new JTextArea();
		configureTriviaArea();	
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
		add(myTriviaArea, FlowLayout.CENTER);
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
		myTriviaArea.setText(theTrivia.getQuestion());
		mySetupTimer.start();
	}
	
	public void processResponse(final String theResponse) {
		if (myCurrentTrivia.isCorrect(theResponse)) {
			myTriviaArea.setText(CORRECT);
			Maze.getInstance().removeTavern();
		} else {
			myTriviaArea.setText(INCORRECT);
			PlayPanel.getInstance().initializeHeartBeat();
			Player.getInstance().decrementHealth();
		}
		myTeardownTimer.start();
	}
	
	private void tearDownTrivia() {
		myTriviaArea.setVisible(false);
		revalidate();
		PlayPanel.getInstance().clearAnswerPanel();
		myDisplayingTrivia = false;
		MazePanel.getInstance().restoreVisibility(myCurrentTrivia.isAnswered());
	}
	
	private void configureTriviaArea() {
		myTriviaArea.setEditable(false);
		myTriviaArea.setPreferredSize(new Dimension(WIDTH - 40, HEIGHT / 3));
		myTriviaArea.setBorder(BorderFactory.createLineBorder(Color.BLACK, 3));
		myTriviaArea.setBackground(Color.WHITE);
		myTriviaArea.setFont(FONT);
		myTriviaArea.setLineWrap(true);
		myTriviaArea.setWrapStyleWord(true);
		myTriviaArea.setEditable(false);
		myTriviaArea.setFocusable(false);
		myTriviaArea.setVisible(false);
	}
	
	private void displayTrivia() {
		myTriviaArea.setVisible(true);
		revalidate();
		PlayPanel.getInstance().updateAnswerPanel(myCurrentTrivia.getTriviaType());
	}
	
}
