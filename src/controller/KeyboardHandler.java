package controller;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import model.Movement;
import view.GamePanel;

public class KeyboardHandler extends KeyAdapter {
	
	private final GamePanel myPanel;
	
	public KeyboardHandler(final GamePanel thePanel) {
		myPanel = thePanel;
	}

	@Override
	public void keyPressed(final KeyEvent theEvent) {
		switch (theEvent.getKeyCode()) {
		case KeyEvent.VK_W:
			myPanel.initializeAdvancement(Movement.UP);
			break;
			
		case KeyEvent.VK_A:
			myPanel.initializeAdvancement(Movement.LEFT);
			break;
			
		case KeyEvent.VK_S:
			myPanel.initializeAdvancement(Movement.DOWN);
			break;
			
		case KeyEvent.VK_D:
			myPanel.initializeAdvancement(Movement.RIGHT);
			break;
		}
	}
}
