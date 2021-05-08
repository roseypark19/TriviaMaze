package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import model.Movement;
import model.Player;

public class PlayPanel extends JPanel {
	
	private static final int WIDTH = 530;
	private static final int HEIGHT = MazePanel.HEIGHT;
	private static final Color TRANSPARENT = new Color(0, 0, 0, 0);
	private static PlayPanel uniqueInstance = new PlayPanel();
	private final Set<KeyPadButton> myKeyButtons;
	private final List<JLabel> myHearts;
//	private final JPanel myNorthPanel;
//	private final JPanel myCenterPanel;
	private final JPanel mySouthPanel;
	
	private PlayPanel() {
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		setBackground(Color.LIGHT_GRAY);
//		setBorder(BorderFactory.createLineBorder(Color.RED));
		setLayout(new BorderLayout());
		myKeyButtons = new HashSet<>();
		myHearts = new ArrayList<>();
		mySouthPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0));
		configureSouthPanel();
		add(mySouthPanel, BorderLayout.SOUTH);
	}
	
	public static synchronized PlayPanel getInstance() {
		return uniqueInstance;
	}
	
	private void configureNorthPanel() {
		// to be added
	}
	
	private void configureCenterPanel() {
		// to be added
	}
	
	private void configureSouthPanel() {
		mySouthPanel.setPreferredSize(new Dimension(WIDTH, 225));
		mySouthPanel.setBackground(TRANSPARENT);
//		mySouthPanel.setBorder(BorderFactory.createLineBorder(Color.BLUE));
		mySouthPanel.add(getKeyPanel());
		updateKeyButtons();
		mySouthPanel.add(Box.createHorizontalStrut(5));
		addHearts();
		updateHearts();
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
			b.addActionListener(theEvent -> 
			              MazePanel.getInstance().initializeAdvancement(b.getMovement()));
			gbc.gridx = gridXs[index];
			gbc.gridy = gridYs[index];
			buttonPanel.add(b, gbc);
			index++;
		}
//		buttonPanel.setBorder(BorderFactory.createLineBorder(Color.GREEN));
		return buttonPanel;
	}
	
	private void addHearts() {
		for (int count = 1; count <= Player.MAX_HEALTH; count++) {
			final JLabel heart = new JLabel(new ImageIcon("heart.png"));
			heart.setPreferredSize(new Dimension(50, 50));
			myHearts.add(heart);
			mySouthPanel.add(heart);
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
		for (int count = Player.MAX_HEALTH; count > hp; count--) {
			myHearts.get(count - 1).setEnabled(false);
		}
		for (int count = 1; count <= hp; count++) {
			myHearts.get(count - 1).setEnabled(true);
		}
	}
}