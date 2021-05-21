package controller;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import model.Movement;
import view.MazePanel;

public class KeyboardHandler extends KeyAdapter {
	
	private boolean myReleased;
	
	public KeyboardHandler() {
		myReleased = true;
	}

	@Override
	public void keyTyped(final KeyEvent theEvent) {
		if (myReleased) {
			MazePanel.getInstance().initializeAdvancement(
			          Movement.valueof(Character.toUpperCase(theEvent.getKeyChar())));
			myReleased = false;
		}	
	}
	
	@Override
	public void keyReleased(final KeyEvent theEvent) {
		myReleased = true;
	}

}
