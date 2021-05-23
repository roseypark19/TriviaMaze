package view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.image.BufferedImage;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.Timer;

import model.Maze;
import model.Player;
import model.Trivia;
import model.TriviaType;
import utilities.SpriteUtilities;

public class TriviaPanel extends JPanel {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1600296983489784055L;
	// images to be added later
	private static final BufferedImage TRIVIA_IMAGE = SpriteUtilities.getTriviaBackground();
	private static final String CORRECT = "Ye got me this time! I won't let you off so easy next time.";
	private static final String INCORRECT = "Argh... not quite! Better luck next time.";
	private static final Font FONT = new Font(Font.MONOSPACED, Font.BOLD, 20);
	private static final int WIDTH = 475;
	private static final int HEIGHT = 595;
	private static final int BORDER_WIDTH = 4;
	private final Timer mySetupTimer;
	private final Timer myTeardownTimer;
	private final JTextArea myTriviaArea;
	private Player myPlayer;
	private Maze myMaze;
	private MazePanel myMazePanel;
	private PlayPanel myPlayPanel;
	private Trivia myCurrentTrivia;
	private BufferedImage myImage;
	private boolean myDisplayingTrivia;
	
	public TriviaPanel(final Player thePlayer, final Maze theMaze) {
		myTriviaArea = new JTextArea();
		myImage = null;
		myPlayer = thePlayer;
		myMaze = theMaze;
		configureTriviaArea();	
		myDisplayingTrivia = false;
		mySetupTimer = new Timer(0, theEvent -> displayTrivia());
		mySetupTimer.setInitialDelay(800);
		mySetupTimer.setRepeats(false);
		myCurrentTrivia = null;
		myTeardownTimer = new Timer(0, theEvent -> tearDownTrivia());
		myTeardownTimer.setInitialDelay(1200);
		myTeardownTimer.setRepeats(false);
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setFocusable(false);
		setBorder(BorderFactory.createLineBorder(Color.BLACK, BORDER_WIDTH));
		add(Box.createRigidArea(new Dimension(WIDTH, 8)));
		add(myTriviaArea, FlowLayout.CENTER);
	}
	
	public void connectPanels(final MazePanel theMazePan, final PlayPanel thePlayPan) {
		if (myMazePanel != null || myPlayPanel != null) {
			throw new IllegalStateException("Panel connections have already been made!");
		}
		myMazePanel = theMazePan;
		myPlayPanel = thePlayPan;
	}
	
	public boolean isAsking() {
		return myDisplayingTrivia;
	}
	
	public void setupNewTrivia(final Trivia theTrivia) {
		myDisplayingTrivia = true;
		myImage = TRIVIA_IMAGE;
		myCurrentTrivia = theTrivia;
		String question = myCurrentTrivia.getQuestion();
		if (myCurrentTrivia.getTriviaType() == TriviaType.MULTICHOICE) {
			question += "\n" + myCurrentTrivia.getAnswers();
		}
		myTriviaArea.setText(question);
		mySetupTimer.start();
	}
	
	public void processResponse(final String theResponse) {
		myMazePanel.grabFocus();
		if (myCurrentTrivia.isCorrect(theResponse)) {
			myTriviaArea.setText(CORRECT);
			myMaze.removeTavern();
		} else {
			myTriviaArea.setText(INCORRECT);
			myPlayPanel.initializeHeartBeat();
			myPlayer.decrementHealth();
			myPlayer.probeHealthState();
		}
		myTeardownTimer.start();
	}
	
	@Override
	public void paintComponent(final Graphics theGraphics) {
		super.paintComponent(theGraphics);
		final Graphics2D g2d = (Graphics2D) theGraphics;
		g2d.drawImage(myImage, null, 0, 0);
	}
	
	private void tearDownTrivia() {
		myImage = null;
		repaint();
		myTriviaArea.setVisible(false);
		revalidate();
		myPlayPanel.clearAnswerPanel();
		myDisplayingTrivia = false;
		myMazePanel.restoreVisibility(myCurrentTrivia.isAnswered());
	}
	
	private void configureTriviaArea() {
		myTriviaArea.setEditable(false);
		myTriviaArea.setPreferredSize(new Dimension(WIDTH - 40, HEIGHT / 3));
		myTriviaArea.setMargin(new Insets(5, 5, 5, 5));
		myTriviaArea.setBackground(Color.WHITE);
		myTriviaArea.setFont(FONT);
		myTriviaArea.setLineWrap(true);
		myTriviaArea.setWrapStyleWord(true);
		myTriviaArea.setEditable(false);
		myTriviaArea.setFocusable(false);
		myTriviaArea.setVisible(false);
	}
	
	private void displayTrivia() {
		repaint();
		myTriviaArea.setVisible(true);
		revalidate();
		myPlayPanel.updateAnswerPanel(myCurrentTrivia.getTriviaType());
	}
	
}
